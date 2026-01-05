# frozen_string_literal: true

require 'sketchup'
require 'extensions'

# Main module for the SketchX SketchUp plugin providing fillet/rounding functionality.
module SketchX
  # ============================================================================
  # CONFIGURATION & CONSTANTS
  # ============================================================================

  # Default Configuration
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

  # FilletTool Class
  class FilletTool
    attr_reader :mode, :radius, :segments, :remove_lead

    # ==========================================================================
    # INITIALIZATION & STATE MANAGEMENT
    # ==========================================================================
    # :single :multi
    #
    def initialize(mode = :single)
      @mode = mode
      @radius = Config::DEFAULT_RADIUS
      @segments = Config::DEFAULT_SEGMENTS
      @remove_lead = true
      @state = :hover
      @ip = Sketchup::InputPoint.new

      reset_selection_data
    end

    def activate
      update_ui
      Sketchup.active_model.selection.clear
    end

    def deactivate(view)
      view.invalidate
    end

    def resume(view)
      update_ui
      view.invalidate
    end

    def get_id
      # Returns a consistent unique integer based on the class name
      self.class.name.hash.abs
    end

    # ==========================================================================
    # UI & STATUS BAR
    # ==========================================================================

    def update_ui
      # 1. Keep the VCB Label simple (helps SketchUp refresh it reliably)
      Sketchup.vcb_label = 'Radius; Segments'

      # 2. Show the values (Radius and Segments)
      Sketchup.vcb_value = "#{@radius}; #{@segments}s"

      # 3. Use the Status Bar for the "Toggle" feedback
      # This is where SketchUp users expect to see tool instructions
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

      # Combine them into one clear instruction line
      full_status = "#{mode_text} | Option/Ctrl: Toggle -> #{lead_text}"
      Sketchup.set_status_text(full_status)
    end

    # ==========================================================================
    # INPUT HANDLING
    # ==========================================================================

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

    def onLButtonDown(flags, x, y, view)
      # NEW: If we are in 'committed' mode, a click means "I'm done editing, start new".
      if @state == :committed
        reset_state
        # After reset, @state is now :hover.
        # We immediately run the hover logic for the current click position
        # so the user doesn't have to click twice.
        @ip.pick(view, x, y)
        @mode == :multi ? handle_hover_multi_round(view) : handle_hover_single_round(view)
      end

      # Existing logic handles the transition from Hover -> Drag -> Commit
      case @state
      when :hover
        @state = :dragging unless @preview_data.empty?
      when :dragging
        commit_geometry
      end
      view.invalidate
    end

    def onUserText(text, view)
      # 1. Parse user input single or double value
      parse_user_input(text)

      # 2. Handle based on State
      if @state == :committed
        # --- NATIVE BEHAVIOR LOGIC ---
        # The user typed a value AFTER the geometry was created.
        # We must UNDO the last operation to restore the original edges,
        # then re-apply the operation with the new radius.
        # But only if we have the data to do so
        if @face_corners_raw.any?
          Sketchup.undo # Revert to before commit_geometry

          # Since we just Undid, so theoriginal edges (@edge_a, @edge_b, etc.) are valid again.
          # We just need to update the preview math with the new @radius.
          recalc_previews

          # And re-commit immediately
          commit_geometry
        end

      elsif @state == :dragging
        # Just update the ghost preview
        recalc_previews unless @face_corners_raw.empty?
        commit_geometry
      end

      view.invalidate
    rescue StandardError => e
      UI.beep
      puts "Input error: #{e.message}"
    end

    def onKeyDown(key, repeat, flags, view)
      # COPY_MODIFIER_MASK captures Cmd on Mac and Ctrl on Windows
      return unless (flags & COPY_MODIFIER_MASK) != 0

      @remove_lead = !@remove_lead
      update_ui
      view.invalidate
      puts "Lead Removal: #{@remove_lead}"
    end

    # Standard SketchUp Callback for Tool Interruption
    def onCancel(reason, view)
      case reason
      when 0 # Escape
        reset_state
        view.invalidate
      when 1 # Changed tool
        reset_selection_data
      when 2 # Undo/Redo
        reset_state
      end
      view.invalidate
    end

    # ==========================================================================
    # INPUT PARSING
    # ==========================================================================

    def parse_user_input(text)
      parts = text.split(';').map(&:strip)

      case parts.length
      when 1
        parse_single_value(parts[0])
      when 2
        parse_double_value(parts[0], parts[1])
      end
    end

    def parse_single_value(value)
      if value.end_with?('s')
        @segments = value.to_i
      else
        @radius = value.to_l
      end
    end

    def parse_double_value(r_str, s_str)
      @radius = r_str.to_l unless r_str.empty?
      @segments = s_str.to_i unless s_str.empty?
    end

    # ==========================================================================
    # HOVER LOGIC
    # ==========================================================================

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

    def recalc_previews
      @preview_data = @face_corners_raw.map do |c|
        FilletCalculator.calc_round(c[:pos_a], c[:pos_b], c[:pos_c], @radius)
      end.compact
    end

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

    def commit_geometry
      return if @preview_data.empty?

      model = Sketchup.active_model
      # 1. Start Operation (Undo from here)
      model.start_operation('Round Selection', true)

      @tangent_edges = []

      # 2. Create arcs and collect tangent edges or leads
      @preview_data.each do |d|
        arc = model.active_entities.add_arc(
          d[:pos_o], d[:xaxis], d[:normal],
          d[:radius], 0.0, d[:o_angle], @segments
        )
        collect_tangent_edges(d, arc)
      end

      # 3. Remove leads or tangent edges if enabled
      remove_leads(@tangent_edges) if @remove_lead

      # 4. Commit Operation
      model.commit_operation

      # 5. CRITICAL CHANGE: Do NOT reset_state yet.
      # Switch to :committed so onUserText knows we just finished an action.
      @state = :committed

      # Update status to let user know they can still type
      Sketchup.set_status_text('Type new radius to modify | Move mouse or press Space to finish.')
      # Update the UI to show the new status
      update_ui

      # Update Selection AFTER commit so the UI can draw it
      # selection = model.selection
      # selection.clear
      # selection.add(@tangent_edges)
      # model.active_view.invalidate

      # Delay or remove reset_state to keep the selection visible
      # reset_state
    end

    def collect_tangent_edges(data, arc)
      model = Sketchup.active_model
      entities = model.active_entities

      # Find edges at tangent points
      corner_point = data[:pos_b]

      entities.grep(Sketchup::Edge).each do |edge|
        # If any end is same between two edge, its tangen edge
        @tangent_edges << edge if edge.start.position == corner_point || edge.end.position == corner_point
      end

      @tangent_edges
    end

    def remove_leads(edges)
      return if edges.empty?

      entities = Sketchup.active_model.active_entities

      # Filter out arc edges and duplicates
      edges_to_remove = edges.uniq.reject { |e| e.deleted? || e.curve }

      # Remove all lead edge connected to vertex corner
      entities.erase_entities(edges_to_remove)
    end

    def reset_state
      @state = :hover
      reset_selection_data
      activate
    end

    # ==========================================================================
    # HELPER METHODS
    # ==========================================================================

    def find_nearest_vertex(edge)
      if edge.start.position.distance(@ip.position) < edge.end.position.distance(@ip.position)
        edge.start
      else
        edge.end
      end
    end

    def find_connected_edge(vertex, exclude_edge)
      vertex.edges.reject { |e| e == exclude_edge }.first
    end

    # ==========================================================================
    # DRAWING
    # ==========================================================================

    def draw(view)
      draw_highlights(view)
      draw_corner_indicators(view)
      draw_previews(view) if @state == :dragging
    end

    # Draws highlighted edges for single and multi round modes
    def draw_highlights(view)
      # Draw highlighted edges for multi round mode
      if @mode == :multi && !@highlight_edges.empty?
        view.line_width = 4
        view.drawing_color = Config::COLORS[:highlight]
        @highlight_edges.each do |e|
          # FIX: Skip if the edge was deleted by the previous commit
          next if e.deleted?

          view.draw(GL_LINES, e.start.position, e.end.position)
        end
      end

      return unless @mode == :single && @edge_a
      # FIX: Skip if single mode edges are deleted
      return if @edge_a.deleted? || @edge_b.deleted?

      # Draw highlighted edges for single round mode
      view.line_width = 3
      view.drawing_color = Config::COLORS[:highlight]
      view.draw(GL_LINES, @edge_a.start.position, @edge_a.end.position)
      view.draw(GL_LINES, @edge_b.start.position, @edge_b.end.position)
    end

    # Draw indicator red dots at corners
    def draw_corner_indicators(view)
      return if @face_corners_raw.empty?

      pts = @face_corners_raw.map { |c| c[:pos_b] }
      view.line_width = 5
      view.draw_points(pts, 10, 2, Config::COLORS[:corner])
    end

    # Draws the preview arcs for the current radius and segments
    def draw_previews(view)
      return if @preview_data.empty?

      HUDRenderer.draw_hud(view, @ip.position, @radius, @segments)
      @preview_data.each { |d| PreviewRenderer.draw_preview(view, d, @segments) }
    end
  end

  # ============================================================================
  # GEOMETRY ANALYZER MODULE
  # ============================================================================

  module GeometryAnalyzer
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

  # ============================================================================
  # FILLET CALCULATOR MODULE
  # ============================================================================

  module FilletCalculator
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

  # ============================================================================
  # CHAIN FINDER MODULE
  # ============================================================================

  module ChainFinder
    def self.find_chain(start_edge)
      return start_edge.curve.edges if start_edge.curve

      chain = [start_edge]

      [start_edge.start, start_edge.end].each do |start_v|
        extend_chain(chain, start_edge, start_v)
      end

      chain
    end

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

  # ============================================================================
  # PREVIEW RENDERER MODULE
  # ============================================================================

  module PreviewRenderer
    def self.draw_preview(view, data, segments)
      arc_depth = calculate_arc_depth(view)

      draw_guide_lines(view, data, arc_depth)
      draw_center_point(view, data)
      draw_arc_preview(view, data, segments, arc_depth)
    end

    def self.calculate_arc_depth(view)
      normal_arc_to_camera = view.camera.direction.reverse
      arc_depth = normal_arc_to_camera
      arc_depth.length = Config::ARC_LIFT_OFFSET
      arc_depth
    end

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

    def self.draw_center_point(view, data)
      view.line_width = 2
      view.draw_points([data[:pos_o]], 6, 2, Config::COLORS[:guide])
    end

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

  # ============================================================================
  # HUD RENDERER MODULE
  # ============================================================================

  module HUDRenderer
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

    def self.calculate_box_points(bounds, padding)
      left = bounds.upper_left.x - padding
      top = bounds.upper_left.y - padding
      right = left + bounds.width + (padding * 2)
      bottom = top + bounds.height + (padding * 2)

      [[left, top], [right, top], [right, bottom], [left, bottom]]
    end

    def self.draw_background(view, bounds, scale_factor)
      pad = Config::HUD_PADDING_SCALE * scale_factor
      box_pts = calculate_box_points(bounds, pad)

      view.drawing_color = Config::COLORS[:hud_bg]
      view.draw2d(GL_POLYGON, box_pts)
    end

    def self.draw_border(view, bounds, scale_factor)
      pad = Config::HUD_PADDING_SCALE * scale_factor
      box_pts = calculate_box_points(bounds, pad)

      view.line_width = 1
      view.drawing_color = Config::COLORS[:hud_border]
      view.draw2d(GL_LINE_LOOP, box_pts)
    end

    def self.draw_text(view, position, text, font_options)
      view.drawing_color = Config::COLORS[:hud_text]
      view.draw_text(position, text, font_options)
    end
  end

  # ============================================================================
  # PLUGIN LOADER & UI
  # ============================================================================

  module PluginLoader
    def self.setup
      create_commands
      create_menus
      create_toolbar
    end

    # Helper method to safely check if our tool is active
    def self.validate_tool_state(mode)
      model = Sketchup.active_model
      # Check if model exists and has a tool manager (prevents crash on exit)
      if model && model.tools
        active_tool = model.tools.active_tool
        return MF_CHECKED if active_tool.is_a?(SketchX::FilletTool) && active_tool.mode == mode
      end
      MF_ENABLED
    rescue StandardError
      MF_ENABLED # Fallback for any unexpected state during shutdown
    end

    def self.create_commands
      # SINGLE ROUND TOOL
      @cmd_single = UI::Command.new('Single Round') do
        Sketchup.active_model.select_tool(SketchX::FilletTool.new(:single))
      end
      @cmd_single.set_validation_proc { validate_tool_state(:single) }
      @cmd_single.small_icon = 'x_single_round.pdf'
      @cmd_single.large_icon = 'x_single_round.pdf'
      @cmd_single.tooltip = 'Round 2 Edges'

      # MULTI ROUND TOOL
      @cmd_multi = UI::Command.new('Multi Round') do
        Sketchup.active_model.select_tool(SketchX::FilletTool.new(:multi))
      end
      @cmd_multi.set_validation_proc { validate_tool_state(:multi) }
      @cmd_multi.small_icon = 'x_multi_round.pdf'
      @cmd_multi.large_icon = 'x_multi_round.pdf'
      @cmd_multi.tooltip = 'Round Multiple Edges'

      # RELOAD TOOL
      @cmd_reload = UI::Command.new('Reload') do
        load __FILE__
        SKETCHUP_CONSOLE.clear
        puts 'RoundMe Reloaded'
      end
      @cmd_reload.small_icon = 'x_reload.pdf'
      @cmd_reload.large_icon = 'x_reload.pdf'
      @cmd_reload.tooltip = 'Reload Plugin'
    end

    def self.create_menus
      menu = UI.menu('Plugins').add_submenu('RoundMe')
      menu.add_item(@cmd_single)
      menu.add_item(@cmd_multi)
      menu.add_item(@cmd_reload)
    end

    def self.create_toolbar
      tb = UI::Toolbar.new('RoundMe')
      tb.add_item(@cmd_single)
      tb.add_item(@cmd_multi)
      tb.add_item(@cmd_reload)
      tb.show
    end
  end

  # ============================================================================
  # INITIALIZATION
  # ============================================================================

  unless file_loaded?(__FILE__)
    PluginLoader.setup
    file_loaded(__FILE__)
  end
end
