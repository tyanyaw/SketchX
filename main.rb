# frozen_string_literal: true

require 'sketchup'
require 'extensions'

# Main module for the SketchX SketchUp plugin providing fillet/rounding functionality.
#
# This module contains the core FilletTool implementation and supporting modules
# for geometry analysis, preview rendering, and HUD display. The plugin allows
# users to create smooth rounded corners on edges with customizable radius and
# segment count.
#
# @since 1.0.0
module SketchX
  # RoundTool module containing all the logic for rounding corners.
  module RoundTool
    # ============================================================================
    # CONFIGURATION & CONSTANTS
    # ============================================================================

    # Configuration module containing all default values and color definitions.
    #
    # This module centralizes configuration constants for the fillet/rounding tool,
    # including geometry constraints, UI styling, and default behavior parameters.
    # All values use SketchUp's length units (mm) where applicable.
    #
    # @since 1.0.0
    module Config
      DEFAULT_RADIUS = 50.mm unless defined?(DEFAULT_RADIUS)
      DEFAULT_SEGMENTS = 12 unless defined?(DEFAULT_SEGMENTS)
      MIN_RADIUS = 0.1.mm unless defined?(MIN_RADIUS)
      MIN_ANGLE = 0.01 unless defined?(MIN_ANGLE)
      MAX_ANGLE = Math::PI - 0.01 unless defined?(MAX_ANGLE)
      ARC_LIFT_OFFSET = 0.2.mm unless defined?(ARC_LIFT_OFFSET)
      HUD_PADDING_SCALE = 3 unless defined?(HUD_PADDING_SCALE)
      HUD_FONT_SIZE = 10 unless defined?(HUD_FONT_SIZE)
      LEAD_TOLERANCE = 0.1.mm unless defined?(LEAD_TOLERANCE)
      DEBUG_MODE = false unless defined?(DEBUG_MODE)

      unless defined?(COLORS)
        COLORS = {
          highlight: 'blue',
          corner: 'red',
          preview: 'orange',
          guide: 'gray',
          hud_bg: [255, 255, 255, 230],
          hud_border: [180, 180, 180],
          hud_text: 'black'
        }.freeze
      end
    end

    # ============================================================================
    # MAIN TOOL CLASS
    # ============================================================================

    # Main tool class for performing fillet/rounding operations on edges.
    #
    # The FilletTool manages the lifecycle of a rounding operation, from initial
    # edge selection (hover) to dynamic radius adjustment (dragging) and final
    # geometry creation (committed). It supports both single-corner and
    # chain-of-edges (multi) modes.
    #
    # @note **State Flow Logic:**
    #   1. **:hover** - The default state. The tool searches for valid corners under the
    #      mouse cursor and generates a temporary "ghost" preview.
    #   2. **:dragging** - Triggered after the first `onLButtonDown`. The tool locks the
    #      selection and dynamically adjusts the `@radius` based on mouse distance.
    #   3. **:committed** - Triggered after the geometry is created. In this state,
    #      the tool remains "attached" to the last operation, allowing the user to
    #      type new values into the VCB to Undo/Redo the fillet with a new radius.
    #
    # @attr_reader mode [Symbol] The current operation mode: `:single` or `:multi`.
    # @attr_reader radius [Length] The current fillet radius in SketchUp length units.
    # @attr_reader segments [Integer] The number of segments used to generate the arc.
    # @attr_reader remove_lead [Boolean] Whether to delete the original corner edges after rounding.
    class FilletTool
      attr_reader :mode, :radius, :segments, :remove_lead

      # ==========================================================================
      # INITIALIZATION & STATE MANAGEMENT
      # ==========================================================================

      # Initializes a new FilletTool instance with specified mode.
      #
      # @param mode [Symbol] Tool operation mode - :single for single corner or :multi for multiple edges
      # @return [FilletTool] A new instance of FilletTool
      def initialize(mode = :single)
        @mode = mode
        @radius = Config::DEFAULT_RADIUS
        @segments = Config::DEFAULT_SEGMENTS
        @remove_lead = true
        @state = :hover
        @ip = Sketchup::InputPoint.new

        reset_selection_data
      end

      # Activates the tool and prepares the SketchUp environment.
      # Clears the current selection and updates UI elements.
      #
      # @return [void]
      def activate
        update_ui
        Sketchup.active_model.selection.clear
      end

      # Deactivates the tool and cleans up the view.
      #
      # @param view [Sketchup::View] The active SketchUp view
      # @return [void]
      def deactivate(view)
        view.invalidate
      end

      # Resumes the tool after being temporarily inactive.
      # Updates UI and refreshes the view.
      #
      # @param view [Sketchup::View] The active SketchUp view
      # @return [void]
      def resume(view)
        update_ui
        view.invalidate
      end

      # Returns a consistent unique integer identifier for this tool.
      # Used by SketchUp to track tool instances.
      #
      # @return [Integer] Unique tool identifier based on class name hash
      def get_id
        self.class.name.hash.abs
      end

      # ==========================================================================
      # UI & STATUS BAR
      # ==========================================================================

      # Updates the user interface elements including VCB (Value Control Box) and status bar.
      # Displays current radius, segments, and contextual instructions based on tool state.
      #
      # @return [void]
      def update_ui
        Sketchup.vcb_label = 'Radius; Segments'
        Sketchup.vcb_value = "#{@radius}; #{@segments}s"

        lead_text = @remove_lead ? '[ Corner : REMOVED ]' : '[ Corner : KEEP ]'

        mode_text = case @state
                    when :hover
                      @mode == :multi ? 'Hover Multi Edges / Face' : 'Hover Corner'
                    when :dragging
                      'Drag to Resize | Type value to commit | Click to Finish'
                    when :committed
                      'Type new value to modify | Click to Finish'
                    else
                      'Ready'
                    end

        full_status = "#{mode_text} | Option/Ctrl: Toggle -> #{lead_text}"
        Sketchup.set_status_text(full_status)
      end

      # ==========================================================================
      # INPUT HANDLING
      # ==========================================================================

      # Handles mouse movement events and updates tool state accordingly.
      # Processes hover, drag, and preview logic based on current state.
      #
      # @param flags [Integer] Modifier key flags
      # @param x [Integer] Screen X coordinate
      # @param y [Integer] Screen Y coordinate
      # @param view [Sketchup::View] The active SketchUp view
      # @return [void]
      def onMouseMove(flags, x, y, view)
        @ip.pick(view, x, y)

        case @state
        when :hover
          @mode == :multi ? handle_hover_multi_round(view) : handle_hover_single_round(view)
        when :dragging
          handle_drag_logic(view)
        end

        update_ui
        view.invalidate
      end

      # Called by SketchUp to determine which cursor to display.
      #
      # @return [void]
      def onSetCursor
        # Native SketchUp Cursor IDs:
        # - 632 Pencil
        # - 633 Panah
        # - 634 Panah +
        # - 636 Panah -
        # - 637 Draw Rect
        # - 638 Ruler
        # - 639 Push
        # - 641 Move X
        # - 643 Reload
        # - 645 Eraser
        # - 646 Offset
        # - 647 Paint
        # - 651 EyeDropper
        # - 668 Hand
        # - 670 Hand Tunjuk
        # - 671 Hand All
        # - 679 Offset No
        # - 680 Paint No
        # - 681 Paint
        case @state
        when :hover
          UI.set_cursor(670)
        when :dragging
          UI.set_cursor(668)
        when :committed
          UI.set_cursor(671)
        else
          UI.set_cursor(633)
        end
      end

      # Handles left mouse button down events.
      # Manages state transitions between hover, dragging, and committed states.
      #
      # @param flags [Integer] Modifier key flags
      # @param x [Integer] Screen X coordinate
      # @param y [Integer] Screen Y coordinate
      # @param view [Sketchup::View] The active SketchUp view
      # @return [void]
      def onLButtonDown(flags, x, y, view)
        if @state == :committed
          reset_state
          @ip.pick(view, x, y)
          @mode == :multi ? handle_hover_multi_round(view) : handle_hover_single_round(view)
        end

        case @state
        when :hover
          @state = :dragging unless @preview_data.empty?
        when :dragging
          commit_geometry
        end
        view.invalidate
      end

      # Processes user text input for radius and segment values.
      # Supports single value (radius or segments) or double value (radius; segments) input.
      # Handles both dragging and committed states with appropriate geometry updates.
      #
      # @param text [String] User input text
      # @param view [Sketchup::View] The active SketchUp view
      # @return [void]
      # @raise [StandardError] If input parsing fails
      def onUserText(text, view)
        parse_user_input(text)

        if @state == :committed
          if @face_corners_raw.any?
            Sketchup.undo
            recalc_previews
            commit_geometry
          end

        elsif @state == :dragging
          recalc_previews unless @face_corners_raw.empty?
          commit_geometry
        end

        view.invalidate
      rescue StandardError => e
        UI.beep
        puts "Input error: #{e.message}"
      end

      # Handles keyboard input for toggling lead removal.
      # Activates when Cmd (Mac) or Ctrl (Windows) key is pressed.
      #
      # @param key [Integer] Key code
      # @param repeat [Integer] Repeat count
      # @param flags [Integer] Modifier key flags
      # @param view [Sketchup::View] The active SketchUp view
      # @return [void]
      def onKeyDown(key, repeat, flags, view)
        return unless (flags & COPY_MODIFIER_MASK) != 0

        @remove_lead = !@remove_lead
        update_ui
        view.invalidate
        puts "Lead Removal: #{@remove_lead}"
      end

      # Handles tool cancellation events (Escape key, tool change, undo/redo).
      # Resets tool state appropriately based on cancellation reason.
      #
      # @param reason [Integer] Cancellation reason code (0=Escape, 1=Tool change, 2=Undo/Redo)
      # @param view [Sketchup::View] The active SketchUp view
      # @return [void]
      def onCancel(reason, view)
        case reason
        when 0
          reset_state
          view.invalidate
        when 1
          reset_selection_data
        when 2
          reset_state
        end
        view.invalidate
      end

      # ==========================================================================
      # INPUT PARSING
      # ==========================================================================

      # Parses user input text for radius and segment values.
      # Supports both single value and semicolon-separated double value formats.
      #
      # @param text [String] Input text to parse
      # @return [void]
      def parse_user_input(text)
        parts = text.split(';').map(&:strip)

        case parts.length
        when 1
          parse_single_value(parts[0])
        when 2
          parse_double_value(parts[0], parts[1])
        end
      end

      # Parses a single input value as either radius or segments.
      # Values ending with 's' are interpreted as segments, otherwise as radius.
      #
      # @param value [String] The value string to parse
      # @return [void]
      def parse_single_value(value)
        if value.end_with?('s')
          @segments = value.to_i
        else
          @radius = value.to_l
        end
      end

      # Parses double input values for radius and segments.
      # Format: "radius; segments"
      #
      # @param r_str [String] Radius value string
      # @param s_str [String] Segments value string
      # @return [void]
      def parse_double_value(r_str, s_str)
        @radius = r_str.to_l unless r_str.empty?
        @segments = s_str.to_i unless s_str.empty?
      end

      # ==========================================================================
      # HOVER LOGIC
      # ==========================================================================

      # Handles hover logic for single round mode.
      # Detects corner formed by two edges and prepares preview data.
      #
      # @param view [Sketchup::View] The active SketchUp view
      # @return [void]
      def handle_hover_single_round(view)
        reset_selection_data
        picked = @ip.edge
        return unless picked && !picked.curve

        v_near = find_nearest_vertex(picked)
        connected = find_connected_edge(v_near, picked)
        return unless connected && !connected.curve

        data = GeometryAnalyzer.extract_data(picked, connected, v_near)
        return unless data[:valid]

        @edge_a = picked
        @edge_b = connected
        @face_corners_raw = [data]
        @max_limit = data[:max_limit]

        recalc_previews
      end

      # Handles hover logic for multi round mode.
      # Detects edge chains and analyzes all corners in the chain.
      #
      # @param view [Sketchup::View] The active SketchUp view
      # @return [void]
      def handle_hover_multi_round(view)
        picked = @ip.edge
        return if picked && @highlight_edges.include?(picked)

        reset_selection_data
        return unless picked

        chain = ChainFinder.find_chain(picked)
        return if chain.length < 2

        @highlight_edges = chain
        analyze_chain_corners(chain)
        recalc_previews
      end

      # ==========================================================================
      # DRAG LOGIC
      # ==========================================================================

      # Handles drag logic for dynamically adjusting radius.
      # Updates radius based on mouse distance from corner point.
      #
      # @param view [Sketchup::View] The active SketchUp view
      # @return [void]
      def handle_drag_logic(view)
        return if @face_corners_raw.empty?

        closest = @face_corners_raw.min_by { |c| c[:pos_b].distance(@ip.position) }
        if closest
          dist = closest[:pos_b].distance(@ip.position)
          @radius = [[dist, @max_limit].min, Config::MIN_RADIUS].max
        end

        recalc_previews
      end

      # ==========================================================================
      # GEOMETRY ANALYSIS
      # ==========================================================================

      # Analyzes corners in an edge chain and calculates maximum radius limits.
      # Populates @face_corners_raw with valid corner data.
      #
      # @param chain [Array<Sketchup::Edge>] Array of edges forming a chain
      # @return [void]
      def analyze_chain_corners(chain)
        limits = []
        vertices = chain.map(&:vertices).flatten.uniq

        vertices.each do |v|
          conn = v.edges.select { |e| chain.include?(e) }
          next unless conn.length == 2 && conn.none?(&:curve)

          d = GeometryAnalyzer.extract_data(conn[0], conn[1], v)
          next unless d[:valid]

          corner_max = calculate_corner_limit(v, conn, chain)
          d[:max_limit] = corner_max

          @face_corners_raw << d
          limits << corner_max
        end

        @max_limit = limits.min || 0.to_l
      end

      # Calculates the maximum allowable radius for a corner based on adjacent edge lengths.
      # Considers whether adjacent corners exist to prevent overlap.
      #
      # @param vertex [Sketchup::Vertex] The corner vertex
      # @param connected_edges [Array<Sketchup::Edge>] Edges connected to the vertex
      # @param chain [Array<Sketchup::Edge>] Complete edge chain
      # @return [Length] Maximum radius limit in SketchUp length units
      def calculate_corner_limit(vertex, connected_edges, chain)
        edge_limits = connected_edges.map do |e|
          other_v = e.other_vertex(vertex)
          other_conn = other_v.edges.select { |oe| chain.include?(oe) }
          other_end_is_corner = (other_conn.length == 2)
          other_end_is_corner ? e.length / 2.0 : e.length
        end

        edge_limits.min
      end

      # ==========================================================================
      # PREVIEW MANAGEMENT
      # ==========================================================================

      # Recalculates preview data for all corners based on current radius.
      # Updates @preview_data with calculated arc parameters.
      #
      # @return [void]
      def recalc_previews
        @preview_data = @face_corners_raw.map do |c|
          FilletCalculator.calc_round(c[:pos_a], c[:pos_b], c[:pos_c], @radius)
        end.compact
      end

      # Resets all selection and preview data to initial state.
      # Clears edges, highlights, corners, and preview information.
      #
      # @return [void]
      def reset_selection_data
        @edge_a = @edge_b = nil
        @highlight_edges = []
        @face_corners_raw = []
        @preview_data = []
        @tangent_edges = []
      end

      # ==========================================================================
      # GEOMETRY CREATION
      # ==========================================================================

      # Commits the geometry by creating arcs and optionally removing lead edges.
      # Wraps operations in an undo-able transaction and updates tool state.
      #
      # @return [void]
      def commit_geometry
        return if @preview_data.empty?

        model = Sketchup.active_model
        model.start_operation('Round Selection', true)

        @tangent_edges = []

        @preview_data.each do |d|
          arc = model.active_entities.add_arc(
            d[:pos_o], d[:xaxis], d[:normal],
            d[:radius], 0.0, d[:o_angle], @segments
          )
          collect_tangent_edges(d, arc)
        end

        remove_leads(@tangent_edges) if @remove_lead

        model.commit_operation

        @state = :committed

        Sketchup.set_status_text('Type new radius to modify | Move mouse or press Space to finish.')
        update_ui
      end

      # Collects tangent edges connected to the corner point.
      # These edges will be removed if lead removal is enabled.
      #
      # @param data [Hash] Corner data containing position information
      # @param arc [Sketchup::ArcCurve] The created arc curve
      # @return [Array<Sketchup::Edge>] Array of collected tangent edges
      def collect_tangent_edges(data, arc)
        model = Sketchup.active_model
        entities = model.active_entities

        corner_point = data[:pos_b]

        entities.grep(Sketchup::Edge).each do |edge|
          is_at_corner = edge.start.position == corner_point || edge.end.position == corner_point
          # is_at_corner = edge.start.position.distance(corner_point) < 0.001.mm || edge.end.position.distance(corner_point) < 0.001.mm

          # Ensure we don't collect the edges that make up the new arc
          @tangent_edges << edge if is_at_corner && edge.curve != arc
        end

        @tangent_edges
      end

      # Removes lead edges connected to rounded corners.
      # Filters out arc edges and deleted edges before removal.
      #
      # @param edges [Array<Sketchup::Edge>] Edges to potentially remove
      # @return [void]
      def remove_leads(edges)
        return if edges.empty?

        entities = Sketchup.active_model.active_entities

        edges_to_remove = edges.uniq.reject { |e| e.deleted? || e.curve }

        entities.erase_entities(edges_to_remove)
      end

      # Resets tool to initial hover state and clears all data.
      # Re-activates the tool with fresh state.
      #
      # @return [void]
      def reset_state
        @state = :hover
        reset_selection_data
        activate
      end

      # ==========================================================================
      # HELPER METHODS
      # ==========================================================================

      # Finds the vertex on an edge nearest to the current input point.
      #
      # @param edge [Sketchup::Edge] The edge to check
      # @return [Sketchup::Vertex] The nearest vertex
      def find_nearest_vertex(edge)
        if edge.start.position.distance(@ip.position) < edge.end.position.distance(@ip.position)
          edge.start
        else
          edge.end
        end
      end

      # Finds the first edge connected to a vertex, excluding a specified edge.
      #
      # @param vertex [Sketchup::Vertex] The vertex to check
      # @param exclude_edge [Sketchup::Edge] Edge to exclude from results
      # @return [Sketchup::Edge, nil] The connected edge or nil
      def find_connected_edge(vertex, exclude_edge)
        vertex.edges.reject { |e| e == exclude_edge }.first
      end

      # ==========================================================================
      # DRAWING
      # ==========================================================================

      # Main drawing method called by SketchUp to render tool graphics.
      # Delegates to specialized drawing methods based on tool state.
      #
      # @param view [Sketchup::View] The active SketchUp view
      # @return [void]
      def draw(view)
        draw_highlights(view)
        draw_corner_indicators(view)
        draw_previews(view) if @state == :dragging
      end

      # Draws highlighted edges for both single and multi round modes.
      # Visual feedback for selected edges that will be rounded.
      #
      # @param view [Sketchup::View] The active SketchUp view
      # @return [void]
      def draw_highlights(view)
        if @mode == :multi && !@highlight_edges.empty?
          view.line_width = 4
          view.drawing_color = Config::COLORS[:highlight]
          @highlight_edges.each do |e|
            next if e.deleted?

            view.draw(GL_LINES, e.start.position, e.end.position)
          end
        end

        return unless @mode == :single && @edge_a
        return if @edge_a.deleted? || @edge_b.deleted?

        view.line_width = 3
        view.drawing_color = Config::COLORS[:highlight]
        view.draw(GL_LINES, @edge_a.start.position, @edge_a.end.position)
        view.draw(GL_LINES, @edge_b.start.position, @edge_b.end.position)
      end

      # Draws red indicator dots at corner positions.
      # Visual markers showing where rounds will be applied.
      #
      # @param view [Sketchup::View] The active SketchUp view
      # @return [void]
      def draw_corner_indicators(view)
        return if @face_corners_raw.empty?

        pts = @face_corners_raw.map { |c| c[:pos_b] }
        view.line_width = 5
        view.draw_points(pts, 10, 2, Config::COLORS[:corner])
      end

      # Draws preview arcs and HUD for current radius and segments.
      # Only active during dragging state to show real-time feedback.
      #
      # @param view [Sketchup::View] The active SketchUp view
      # @return [void]
      def draw_previews(view)
        return if @preview_data.empty?

        HUDRenderer.draw_hud(view, @ip.position, @radius, @segments)
        @preview_data.each { |d| PreviewRenderer.draw_preview(view, d, @segments) }
      end
    end
    # End FilletTool Class

    # ============================================================================
    # GEOMETRY ANALYZER MODULE
    # ============================================================================

    # GeometryAnalyzer Module to extract geometric data
    module GeometryAnalyzer
      # Extracts geometric data from two edges forming a corner.
      # Calculates positions, vectors, and maximum radius limit.
      #
      # @param edge_a [Sketchup::Edge] First edge
      # @param edge_c [Sketchup::Edge] Second edge
      # @param vtx [Sketchup::Vertex, nil] Corner vertex (optional)
      # @return [Hash] Corner Data Structure:
      #   * :pos_b [Geom::Point3d] The shared corner point.
      #   * :pos_a [Geom::Point3d] End point of first edge.
      #   * :pos_c [Geom::Point3d] End point of second edge.
      #   * :max_limit [Length] Maximum allowable radius for this corner.
      #   * :valid [Boolean] Success flag.
      def self.extract_data(edge_a, edge_c, vtx = nil)
        if vtx
          pos_b = vtx.position
        else
          pos_b = Geom.intersect_line_line(edge_a.line, edge_c.line)
          return { valid: false, error: 'Parallel' } unless pos_b
        end

        pos_a = edge_a.other_vertex(vtx || edge_a.start).position
        pos_c = edge_c.other_vertex(vtx || edge_c.start).position

        vec_a = (pos_a - pos_b).normalize
        vec_c = (pos_c - pos_b).normalize

        len_a = pos_b.distance(pos_a)
        len_c = pos_b.distance(pos_c)
        max_rad = [len_a, len_c].min

        {
          pos_b: pos_b,
          pos_a: pos_a,
          pos_c: pos_c,
          max_limit: max_rad,
          valid: true
        }
      rescue StandardError => e
        { valid: false, error: e.message }
      end
    end
    # End GeometryAnalyzer Module

    # ============================================================================
    # FILLET CALCULATOR MODULE
    # ============================================================================

    # FilletCalculator Module containing memory math calculations
    module FilletCalculator
      # Calculates fillet/round arc parameters for a corner.
      # Computes center point, tangent points, angles, and orientation.
      #
      # @param pos_a [Geom::Point3d] Position of first edge endpoint.
      # @param pos_b [Geom::Point3d] Position of corner point.
      # @param pos_c [Geom::Point3d] Position of second edge endpoint.
      # @param radius [Length] Desired radius for the round.
      #
      # @return [Hash, nil] Mathematical Calculation Data Structure:
      #   * :xaxis [Geom::Vector3d] Unit vector from arc center to first tangent point (pos_p1).
      #   * :yaxis [Geom::Vector3d] Unit vector from arc center to second tangent point (pos_p2).
      #   * :normal [Geom::Vector3d] Vector perpendicular to the arc plane, oriented for correct sweep direction.
      #   * :radius [Length] The radius used for this calculation.
      #   * :vector_ba [Geom::Vector3d] Normalized vector from corner (pos_b) to pos_a.
      #   * :vector_bc [Geom::Vector3d] Normalized vector from corner (pos_b) to pos_c.
      #   * :vector_p1 [Geom::Vector3d] Vector from corner (pos_b) to tangent point pos_p1.
      #   * :vector_p2 [Geom::Vector3d] Vector from corner (pos_b) to tangent point pos_p2.
      #   * :pos_a [Geom::Point3d] Original first edge endpoint.
      #   * :pos_b [Geom::Point3d] Original corner vertex position.
      #   * :pos_c [Geom::Point3d] Original second edge endpoint.
      #   * :pos_o [Geom::Point3d] Calculated center point of the fillet arc.
      #   * :pos_p1 [Geom::Point3d] First tangent point where the arc meets edge BA.
      #   * :pos_p2 [Geom::Point3d] Second tangent point where the arc meets edge BC.
      #   * :b_angle [Float] The interior angle of the corner in radians.
      #   * :o_angle [Float] The central angle (sweep) of the arc in radians.
      #   * :dist_to_center [Float] The distance from the corner point (pos_b) to the arc center (pos_o).
      def self.calc_round(pos_a, pos_b, pos_c, radius)
        vector_ba = (pos_a - pos_b).normalize
        vector_bc = (pos_c - pos_b).normalize

        b_angle = vector_ba.angle_between(vector_bc)
        return nil if b_angle < Config::MIN_ANGLE || b_angle > Config::MAX_ANGLE

        o_angle = Math::PI - b_angle
        dist_to_center = radius / Math.sin(b_angle / 2.0)

        bisector = (vector_ba + vector_bc).normalize
        pos_o = pos_b.offset(bisector, dist_to_center)

        tangent_len = radius / Math.tan(b_angle / 2.0)
        normal = vector_ba.cross(vector_bc).normalize

        pos_p1 = pos_b.offset(vector_ba, tangent_len)
        pos_p2 = pos_b.offset(vector_bc, tangent_len)

        vector_p1 = vector_ba.clone
        vector_p1.length = tangent_len
        vector_p2 = vector_bc.clone
        vector_p2.length = tangent_len

        xaxis = (pos_p1 - pos_o).normalize
        yaxis = (pos_p2 - pos_o).normalize

        test_transform = Geom::Transformation.rotation(pos_o, normal, o_angle)
        test_vector = xaxis.transform(test_transform)
        test_vector.length = radius
        test_pt = pos_o + test_vector
        normal.reverse! if test_pt.distance(pos_p2) > 0.01.mm

        {
          xaxis: xaxis,
          yaxis: yaxis,
          normal: normal,
          radius: radius,
          vector_ba: vector_ba,
          vector_bc: vector_bc,
          vector_p1: vector_p1,
          vector_p2: vector_p2,
          pos_a: pos_a,
          pos_b: pos_b,
          pos_c: pos_c,
          pos_o: pos_o,
          pos_p1: pos_p1,
          pos_p2: pos_p2,
          b_angle: b_angle,
          o_angle: o_angle,
          dist_to_center: dist_to_center
        }
      end
    end
    # End FilletCalculator Module

    # ============================================================================
    # CHAIN FINDER MODULE
    # ============================================================================

    module ChainFinder
      # Finds a continuous chain of connected edges starting from a given edge.
      # Returns curve edges if the start edge belongs to a curve.
      #
      # @param start_edge [Sketchup::Edge] The edge to start chain detection from
      # @return [Array<Sketchup::Edge>] Array of edges forming the chain
      def self.find_chain(start_edge)
        return start_edge.curve.edges if start_edge.curve

        chain = [start_edge]

        [start_edge.start, start_edge.end].each do |start_v|
          extend_chain(chain, start_edge, start_v)
        end

        chain
      end

      # Extends an edge chain by following connected edges from a vertex.
      # Stops when reaching a vertex with more than 2 edges or a dead end.
      #
      # @param chain [Array<Sketchup::Edge>] Current chain of edges
      # @param start_edge [Sketchup::Edge] Initial edge
      # @param start_vertex [Sketchup::Vertex] Vertex to extend from
      # @return [void]
      def self.extend_chain(chain, start_edge, start_vertex)
        curr_v = start_vertex
        curr_e = start_edge

        loop do
          all_connected = curr_v.edges
          break if all_connected.length != 2

          next_e = (all_connected - [curr_e]).first
          break if chain.include?(next_e)

          chain << next_e
          curr_v = next_e.other_vertex(curr_v)
          curr_e = next_e
        end
      end
    end
    # End ChainFinder Module

    # ============================================================================
    # PREVIEW RENDERER MODULE
    # ============================================================================

    # PreviewRenderer Module to draw the preview of the fillet arc.
    module PreviewRenderer
      # Draws complete preview visualization for a fillet arc.
      # Includes guide lines, center point, and arc preview.
      #
      # @param view [Sketchup::View] The active SketchUp view
      # @param data [Hash] Fillet calculation data
      # @param segments [Integer] Number of arc segments to draw
      # @return [void]
      def self.draw_preview(view, data, segments)
        arc_depth = calculate_arc_depth(view)

        draw_guide_lines(view, data, arc_depth)
        draw_center_point(view, data)
        draw_arc_preview(view, data, segments, arc_depth)
      end

      # Calculates depth offset for drawing arc slightly in front of geometry.
      # Prevents z-fighting with existing faces.
      #
      # @param view [Sketchup::View] The active SketchUp view
      # @return [Geom::Vector3d] Offset vector toward camera
      def self.calculate_arc_depth(view)
        normal_arc_to_camera = view.camera.direction.reverse
        arc_depth = normal_arc_to_camera
        arc_depth.length = Config::ARC_LIFT_OFFSET
        arc_depth
      end

      # Draws dashed guide lines from arc center to tangent points.
      #
      # @param view [Sketchup::View] The active SketchUp view
      # @param data [Hash] Fillet calculation data
      # @param arc_depth [Geom::Vector3d] Depth offset vector
      # @return [void]
      def self.draw_guide_lines(view, data, arc_depth)
        p_o_lifted = data[:pos_o].offset(arc_depth)
        p_p1_lifted = data[:pos_p1].offset(arc_depth)
        p_p2_lifted = data[:pos_p2].offset(arc_depth)

        view.line_width = 1
        view.drawing_color = Config::COLORS[:guide]
        view.line_stipple = '.'
        view.draw(GL_LINES, [p_o_lifted, p_p1_lifted])
        view.draw(GL_LINES, [p_o_lifted, p_p2_lifted])
        view.line_stipple = ''
      end

      # Draws a point marker at the arc center position.
      #
      # @param view [Sketchup::View] The active SketchUp view
      # @param data [Hash] Fillet calculation data
      # @return [void]
      def self.draw_center_point(view, data)
        view.line_width = 2
        view.draw_points([data[:pos_o]], 6, 2, Config::COLORS[:guide])
      end

      # Draws the arc preview with specified number of segments.
      # Generates points along the arc and renders as line strip.
      #
      # @param view [Sketchup::View] The active SketchUp view
      # @param data [Hash] Fillet calculation data
      # @param segments [Integer] Number of arc segments
      # @param arc_depth [Geom::Vector3d] Depth offset vector
      # @return [void]
      def self.draw_arc_preview(view, data, segments, arc_depth)
        points = []
        view.drawing_color = Config::COLORS[:preview]
        step = data[:o_angle] / segments
        current_rad = data[:radius]

        (0..segments).each do |i|
          tr = Geom::Transformation.rotation(data[:pos_o], data[:normal], i * step)
          vec = data[:xaxis].transform(tr)
          vec.length = current_rad
          pt = (data[:pos_o] + vec).offset(arc_depth)
          points << pt
        end

        view.draw(GL_LINE_STRIP, points)
      end
    end
    # End PreviewRenderer Module

    # ============================================================================
    # HUD RENDERER MODULE
    # ============================================================================

    # HUDRenderer Module to draw the HUD of the fillet arc.
    module HUDRenderer
      # Draws a heads-up display showing current radius and segment count.
      # Positioned near the input point with background and border.
      #
      # @param view [Sketchup::View] The active SketchUp view
      # @param position [Geom::Point3d] 3D position for HUD anchor
      # @param radius [Length] Current radius value
      # @param segments [Integer] Current segment count
      # @return [void]
      def self.draw_hud(view, position, radius, segments)
        s_factor = UI.scale_factor
        screen_pos = view.screen_coords(position)
        label_pos = [screen_pos.x + (20 * s_factor), screen_pos.y - (20 * s_factor)]
        label_text = "R: #{Sketchup.format_length(radius)} | S: #{segments}"

        font_opts = build_font_options(s_factor)
        bounds = view.text_bounds(label_pos, label_text, font_opts)

        draw_background(view, bounds, s_factor)
        draw_border(view, bounds, s_factor)
        draw_text(view, label_pos, label_text, font_opts)
      end

      # Builds font options hash with platform-specific size key.
      # Handles differences between Windows, macOS, and other platforms.
      #
      # @param scale_factor [Float] UI scale factor for high DPI displays
      # @return [Hash] Font options for text rendering
      def self.build_font_options(scale_factor)
        size_key = case Sketchup.platform
                   when :platform_win then :pixel_size
                   when :platform_osx then :size
                   else :point_size
                   end

        f_size = (Config::HUD_FONT_SIZE * scale_factor).to_i

        {
          font: 'Tahoma',
          size_key => f_size,
          bold: false
        }
      end

      # Calculates 2D bounding box points for HUD background.
      # Adds padding around text bounds.
      #
      # @param bounds [Geom::Bounds2d] Text bounds
      # @param padding [Numeric] Padding in screen pixels
      # @return [Array<Array<Numeric>>] Array of [x, y] coordinate pairs
      def self.calculate_box_points(bounds, padding)
        left = bounds.upper_left.x - padding
        top = bounds.upper_left.y - padding
        right = left + bounds.width + (padding * 2)
        bottom = top + bounds.height + (padding * 2)

        [[left, top], [right, top], [right, bottom], [left, bottom]]
      end

      # Draws semi-transparent background rectangle for HUD.
      #
      # @param view [Sketchup::View] The active SketchUp view
      # @param bounds [Geom::Bounds2d] Text bounds
      # @param scale_factor [Float] UI scale factor
      # @return [void]
      def self.draw_background(view, bounds, scale_factor)
        pad = Config::HUD_PADDING_SCALE * scale_factor
        box_pts = calculate_box_points(bounds, pad)

        view.drawing_color = Config::COLORS[:hud_bg]
        view.draw2d(GL_POLYGON, box_pts)
      end

      # Draws border outline around HUD background.
      #
      # @param view [Sketchup::View] The active SketchUp view
      # @param bounds [Geom::Bounds2d] Text bounds
      # @param scale_factor [Float] UI scale factor
      # @return [void]
      def self.draw_border(view, bounds, scale_factor)
        pad = Config::HUD_PADDING_SCALE * scale_factor
        box_pts = calculate_box_points(bounds, pad)

        view.line_width = 1
        view.drawing_color = Config::COLORS[:hud_border]
        view.draw2d(GL_LINE_LOOP, box_pts)
      end

      # Draws the HUD text with specified font options.
      #
      # @param view [Sketchup::View] The active SketchUp view
      # @param position [Array<Numeric>] Screen [x, y] position
      # @param text [String] Text to display
      # @param font_options [Hash] Font rendering options
      # @return [void]
      def self.draw_text(view, position, text, font_options)
        view.drawing_color = Config::COLORS[:hud_text]
        view.draw_text(position, text, font_options)
      end
    end
    # End HUDRenderer Module

    # ============================================================================
    # PLUGIN LOADER & UI
    # ============================================================================

    # PluginLoader Module to load the plugin and create the UI.
    module PluginLoader
      # Sets up the plugin by creating commands, menus, and toolbar.
      # Called once during plugin initialization.
      #
      # @return [void]
      def self.setup
        create_commands
        create_menus
        create_toolbar
      end

      # Safely validates tool state by checking if the specified mode is active.
      # Returns appropriate menu flag for command validation.
      #
      # @param mode [Symbol] Tool mode to check (:single or :multi)
      # @return [Integer] Menu flag constant (MF_CHECKED or MF_ENABLED)
      def self.validate_tool_state(mode)
        model = Sketchup.active_model
        if model && model.tools
          active_tool = model.tools.active_tool
          return MF_CHECKED if active_tool.is_a?(SketchX::RoundTool::FilletTool) && active_tool.mode == mode
        end
        MF_ENABLED
      rescue StandardError
        MF_ENABLED
      end

      # Creates UI commands for single round, multi round, and reload operations.
      # Configures icons, tooltips, and validation procedures.
      #
      # @return [void]
      def self.create_commands
        @cmd_single = UI::Command.new('Single Round') do
          Sketchup.active_model.select_tool(SketchX::RoundTool::FilletTool.new(:single))
        end
        @cmd_single.set_validation_proc { validate_tool_state(:single) }
        @cmd_single.small_icon = 'x_single_round.pdf'
        @cmd_single.large_icon = 'x_single_round.pdf'
        @cmd_single.tooltip = 'Round 2 Edges'

        @cmd_multi = UI::Command.new('Multi Round') do
          Sketchup.active_model.select_tool(SketchX::RoundTool::FilletTool.new(:multi))
        end
        @cmd_multi.set_validation_proc { validate_tool_state(:multi) }
        @cmd_multi.small_icon = 'x_multi_round.pdf'
        @cmd_multi.large_icon = 'x_multi_round.pdf'
        @cmd_multi.tooltip = 'Round Multiple Edges'

        @cmd_reload = UI::Command.new('Reload') do
          load __FILE__
          SKETCHUP_CONSOLE.clear
          puts 'RoundMe Reloaded'
        end
        @cmd_reload.small_icon = 'x_reload.pdf'
        @cmd_reload.large_icon = 'x_reload.pdf'
        @cmd_reload.tooltip = 'Reload Plugin'
      end

      # Creates plugin menu under Plugins menu.
      # Adds all command items to the submenu.
      #
      # @return [void]
      def self.create_menus
        plugin_menu = UI.menu('Plugins').add_submenu('SketchX')
        menu = plugin_menu.add_submenu('Round Tool')
        menu.add_item(@cmd_single)
        menu.add_item(@cmd_multi)
        menu.add_item(@cmd_reload) if Config::DEBUG_MODE == true
      end

      # Creates and displays the plugin toolbar.
      # Adds all command buttons to the toolbar.
      #
      # @return [void]
      def self.create_toolbar
        tb = UI::Toolbar.new('SketchX')
        tb.add_item(@cmd_single)
        tb.add_item(@cmd_multi)
        tb.add_item(@cmd_reload) if Config::DEBUG_MODE == true
        tb.show
      end
    end
    # End PluginLoader Module

    # ============================================================================
    # INITIALIZATION
    # ============================================================================

    unless file_loaded?(__FILE__)
      PluginLoader.setup
      file_loaded(__FILE__)
    end
    # End initialization
  end
  # End RoundTool
end
# End SketchX
