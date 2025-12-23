# frozen_string_literal: true

require 'sketchup'
require 'extensions'

module RoundMe
  class FilletTool
    # ==========================================================================
    # 1. INITIALIZATION & STATE MANAGEMENT
    # ==========================================================================

    # Initializes the tool state, default parameters, and data containers.
    # Sets up mode (:edge or :face), radius, segments, and input point tracking.
    def initialize(mode = :edge)
      @mode = mode
      @radius = 50.mm
      @segments = 12
      @state = 0
      @ip = Sketchup::InputPoint.new

      @edge_a = nil
      @edge_b = nil

      @hover_face = nil
      @face_corners_raw = []
      @max_limit = 0.to_l

      @preview_data = []
    end

    # Called when the tool is selected.
    # Resets the selection and updates the status bar/VCB.
    def activate
      update_ui
      Sketchup.active_model.selection.clear
    end

    # Updates the VCB label/value and the Status Bar text based on the current state.
    def update_ui
      Sketchup.vcb_label = 'Radius; Segments'
      Sketchup.vcb_value = "#{@radius}; #{@segments}s"

      msg = if @state == 0
              @mode == :face ? "Hover Face (Default #{@radius})" : 'Hover Corner'
            else
              'Drag to Resize. Type value to commit.'
            end
      Sketchup.set_status_text(msg)
    end

    # ==========================================================================
    # 2. INPUT HANDLING (MOUSE & KEYBOARD)
    # ==========================================================================

    # Handles mouse movement for both Hover (State 0) and Drag (State 1) modes.
    # Delegates logic to specific handlers based on the current @mode.
    def onMouseMove(flags, x, y, view)
      @ip.pick(view, x, y)

      if @state == 0
        @mode == :face ? handle_hover_face(view, x, y) : handle_hover_edge(view)
      elsif @state == 1
        @mode == :face ? handle_drag_face(view) : handle_drag_edge(view)
      end

      update_ui
      view.invalidate
    end

    # Handles left mouse click.
    # Locks the selection (State 0 -> 1) or commits geometry (State 1 -> Finish).
    def onLButtonDown(flags, x, y, view)
      if @state == 0
        @state = 1 if (@mode == :edge && !@preview_data.empty?) || (@mode == :face && @hover_face)
      elsif @state == 1
        finalize_tool
      end
    end

    # Parses user input from the VCB (Measurements box).
    # Accepts "radius", "segments" (suffix 's'), or "radius; segments".
    def onUserText(text, view)
      if text.include?(';')
        r_str, s_str = text.split(';')
        @radius = r_str.to_l
        @segments = s_str.to_i
      elsif text.end_with?('s')
        @segments = text.to_i
      else
        @radius = text.to_l
      end

      if @state == 1
        if @mode == :face
          recalc_face_previews
        else
          c = @face_corners_raw.first
          if c
            res = FilletTool.calc_round(c[:pos_a], c[:pos_b], c[:pos_c], @radius)
            @preview_data = res ? [res] : []
          end
        end
        finalize_tool
      end
    rescue StandardError
      UI.beep
    end

    # ==========================================================================
    # 3. INTERACTION LOGIC (HOVER & DRAG)
    # ==========================================================================

    # Identifies two intersecting edges connected to the picked vertex.
    # Populates @edge_a, @edge_b, and @face_corners_raw if valid.
    # Explicitly ignores edges that belong to an existing Curve or Arc.
    def handle_hover_edge(view)
      @edge_a = nil
      @edge_b = nil
      @preview_data = []
      @face_corners_raw = []

      picked = @ip.edge
      return unless picked
      return if picked.curve

      v_near = picked.start.position.distance(@ip.position) < picked.end.position.distance(@ip.position) ? picked.start : picked.end
      connected = v_near.edges.reject { |e| e == picked }.first
      return unless connected
      return if connected.curve

      data = FilletTool.extract_data(picked, connected, v_near)
      return unless data[:valid]

      @edge_a = picked
      @edge_b = connected
      @face_corners_raw = [data]
      @max_limit = data[:max_limit]

      res = FilletTool.calc_round(data[:pos_a], data[:pos_b], data[:pos_c], @radius)
      @preview_data = res ? [res] : []
    end

    # Updates the radius based on mouse distance from the corner vertex.
    # Clamps the radius between 1mm and the maximum geometric limit.
    def handle_drag_edge(view)
      c = @face_corners_raw.first
      dist = c[:pos_b].distance(@ip.position)

      @radius = [dist, @max_limit].min
      @radius = 1.mm if @radius < 1.mm

      res = FilletTool.calc_round(c[:pos_a], c[:pos_b], c[:pos_c], @radius)
      @preview_data = res ? [res] : []
    end

    # Detects the face under the cursor and pre-calculates data for all its corners.
    # Sets @hover_face and populates @face_corners_raw.
    # Skips any corners where one of the edges is a Curve.
    def handle_hover_face(view, x, y)
      ph = view.pick_helper
      ph.do_pick(x, y)
      face = ph.best_picked

      if face != @hover_face
        @hover_face = face.is_a?(Sketchup::Face) ? face : nil
        @face_corners_raw = []
        @preview_data = []

        if @hover_face
          limits = []
          @hover_face.vertices.each do |vtx|
            edges = vtx.edges.select { |e| @hover_face.edges.include?(e) }
            next unless edges.length == 2
            next if edges.any? { |e| e.curve }

            d = FilletTool.extract_data(edges[0], edges[1], vtx)
            if d[:valid]
              @face_corners_raw << d
              limits << d[:max_limit]
            end
          end
          @max_limit = limits.min || 0.to_l
        end
      end

      recalc_face_previews if @hover_face
    end

    # Updates the radius based on the distance from the mouse to the nearest corner.
    def handle_drag_face(view)
      return unless @hover_face

      closest_corner = @face_corners_raw.min_by { |c| c[:pos_b].distance(@ip.position) }

      if closest_corner
        dist = closest_corner[:pos_b].distance(@ip.position)
        @radius = [dist, @max_limit].min
        @radius = 1.mm if @radius < 1.mm
      end

      recalc_face_previews
    end

    # Re-runs the math calculation for all corners on the face using the current radius.
    def recalc_face_previews
      @preview_data = []
      @face_corners_raw.each do |c|
        res = FilletTool.calc_round(c[:pos_a], c[:pos_b], c[:pos_c], @radius)
        @preview_data << res if res
      end
    end

    # ==========================================================================
    # 4. GEOMETRY CREATION (COMMIT)
    # ==========================================================================

    # Commits the preview geometry to the SketchUp model.
    # Wraps the creation in an Undo operation and resets the tool state.
    def finalize_tool
      return if @preview_data.empty?

      model = Sketchup.active_model
      model.start_operation('Round Selection', true)

      @preview_data.each do |data|
        FilletTool.draw_2d_round(model, data, @segments)
      end

      model.commit_operation

      @state = 0
      @preview_data = []
      @face_corners_raw = []
      @hover_face = nil
      @edge_a = nil
      activate
    end

    # Creates the physical arc geometry in the model entities.
    def self.draw_2d_round(model, data, segments)
      model.active_entities.add_arc(
        data[:pos_o], data[:xaxis], data[:normal],
        data[:radius], 0.0, data[:o_angle], segments
      )
    end

    # Helper to process a loop selection (Command entry point).
    def self.loop_selection(selection, radius, segments)
      model = Sketchup.active_model
      corners_to_process = []

      if selection.is_a?(Sketchup::Selection) || selection.is_a?(Array)
        items = selection.grep(Sketchup::Face)
        if items.empty?
          edges = selection.grep(Sketchup::Edge)
          return UI.messagebox('Select a Face or 2 intersecting Edges') if edges.length < 2
          return if edges.any? { |e| e.curve }

          common_vtx = edges[0].vertices.find { |v| edges[1].vertices.include?(v) }
          data = extract_data(edges[0], edges[1], common_vtx)
          corners_to_process << data if data[:valid]
        else
          items.each do |face|
            face.vertices.each do |vtx|
              edges = vtx.edges.select { |e| face.edges.include?(e) }
              next unless edges.length == 2
              next if edges.any? { |e| e.curve }

              data = extract_data(edges[0], edges[1], vtx)
              corners_to_process << data if data[:valid]
            end
          end
        end
      end

      model.start_operation('Round Selection', true)
      corners_to_process.each do |corner|
        math_result = calc_round(corner[:pos_a], corner[:pos_b], corner[:pos_c], radius)
        draw_2d_round(model, math_result, segments) if math_result
      end
      model.commit_operation
    end

    # ==========================================================================
    # 5. MATHEMATICAL HELPERS (STATIC)
    # ==========================================================================

    # Extracts geometric points (A, B, C) and validates the corner topology.
    # Calculates the maximum possible radius (max_limit) for the corner.
    def self.extract_data(edge_a, edge_c, vtx = nil)
      if vtx
        pos_b = vtx.position
      else
        pos_b = Geom.intersect_line_line(edge_a.line, edge_c.line)
        return { valid: false, error: 'Parallel' } unless pos_b
      end

      pos_a = edge_a.other_vertex(vtx || edge_a.start).position
      pos_c = edge_c.other_vertex(vtx || edge_c.start).position

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
    rescue StandardError
      { valid: false }
    end

    # Performs the pure geometric calculations for the fillet arc.
    # Returns a hash of vectors, points, and angles needed for drawing.
    def self.calc_round(pos_a, pos_b, pos_c, radius)
      vector_ba = (pos_a - pos_b).normalize
      vector_bc = (pos_c - pos_b).normalize

      b_angle = vector_ba.angle_between(vector_bc)
      return nil if b_angle < 0.01 || b_angle > (Math::PI - 0.01)

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

    # ==========================================================================
    # 6. DRAW METHODS (VIEW & HUD)
    # ==========================================================================

    # Main draw method called by SketchUp every frame.
    # Coordinates the drawing of static highlights, corner dots, and dynamic previews.
    def draw(view)
      return if @face_corners_raw.nil? || @preview_data.nil?

      if @mode == :edge && @edge_a
        view.line_width = 3
        view.drawing_color = 'blue'
        view.draw(GL_LINES, @edge_a.start.position, @edge_a.end.position)
        view.draw(GL_LINES, @edge_b.start.position, @edge_b.end.position)
      elsif @mode == :face && @hover_face
        view.drawing_color = [0, 0, 255, 60]
        view.draw(GL_POLYGON, @hover_face.outer_loop.vertices.map(&:position))
      end

      unless @face_corners_raw.empty?
        pts = @face_corners_raw.map { |c| c[:pos_b] }
        view.line_width = 5
        view.draw_points(pts, 10, 2, 'red')
      end

      return if @preview_data.empty?

      add_radius_text(view)

      @preview_data.each do |d|
        draw_preview_geometry(view, d)
      end
    end

    # Draws the dynamic elements (dashed lines, center point, and arc) for a single fillet.
    def draw_preview_geometry(view, d)
      normal_arc_to_camera = view.camera.direction.reverse
      arc_depth = normal_arc_to_camera
      arc_depth.length = 0.2.mm

      p_o_lifted  = d[:pos_o].offset(arc_depth)
      p_p1_lifted = d[:pos_p1].offset(arc_depth)
      p_p2_lifted = d[:pos_p2].offset(arc_depth)

      view.line_width = 1
      view.drawing_color = 'gray'
      view.line_stipple = '.'
      view.draw(GL_LINES, [p_o_lifted, p_p1_lifted])
      view.draw(GL_LINES, [p_o_lifted, p_p2_lifted])
      view.line_stipple = ''

      view.line_width = 2
      view.draw_points([d[:pos_o]], 6, 2, 'gray')

      points = []
      view.drawing_color = 'orange'
      step = d[:o_angle] / @segments
      current_rad = d[:radius]

      (0..@segments).each do |i|
        tr = Geom::Transformation.rotation(d[:pos_o], d[:normal], i * step)
        vec = d[:xaxis].transform(tr)
        vec.length = current_rad
        pt = (d[:pos_o] + vec).offset(arc_depth)
        points << pt
      end
      view.draw(GL_LINE_STRIP, points)
    end

    # Renders the floating HUD with radius and segment information.
    # Handles High-DPI scaling and cross-platform font differences.
    def add_radius_text(view)
      return unless @state == 1 && @preview_data.any?

      s_factor = UI.scale_factor
      screen_pos = view.screen_coords(@ip.position)
      label_pos = [screen_pos.x + (20 * s_factor), screen_pos.y - (20 * s_factor)]
      label_text = "R: #{Sketchup.format_length(@radius)} | S: #{@segments}"

      size_key = if Sketchup.platform == :platform_win
                   :pixel_size
                 elsif Sketchup.platform == :platform_osx
                   :size
                 else
                   :point_size
                 end

      f_size = (10 * s_factor).to_i

      font_opts = {
        font: 'Tahoma',
        size_key => f_size,
        bold: false
      }

      bounds = view.text_bounds(label_pos, label_text, font_opts)
      pad = 3 * s_factor

      left   = bounds.upper_left.x - pad
      top    = bounds.upper_left.y - pad
      right  = left + bounds.width + (pad * 2)
      bottom = top + bounds.height + (pad * 2)
      box_pts = [[left, top], [right, top], [right, bottom], [left, bottom]]

      view.drawing_color = [255, 255, 255, 230]
      view.draw2d(GL_POLYGON, box_pts)

      view.line_width = 1
      view.drawing_color = [180, 180, 180]
      view.draw2d(GL_LINE_LOOP, box_pts)

      view.drawing_color = 'black'
      view.draw_text(label_pos, label_text, font_opts)
    end
  end

  # ============================================================================
  # 7. PLUGIN LOADER & MENUS
  # ============================================================================
  unless file_loaded?(__FILE__)
    cmd_single_round = UI::Command.new('Single Round') do
      Sketchup.active_model.select_tool(RoundMe::FilletTool.new(:edge))
    end
    cmd_single_round.small_icon = 'x_round.pdf'
    cmd_single_round.large_icon = 'x_round.pdf'
    cmd_single_round.tooltip = 'Interactive Round Tool'

    cmd_multi_round = UI::Command.new('Multi Round') do
      Sketchup.active_model.select_tool(RoundMe::FilletTool.new(:face))
    end
    cmd_multi_round.small_icon = 'x_round.pdf'
    cmd_multi_round.large_icon = 'x_round.pdf'
    cmd_multi_round.tooltip = 'Round Selected Face'

    cmd_reload = UI::Command.new('Reload') do
      load __FILE__
      puts 'RoundMe Reloaded'
    end
    cmd_reload.small_icon = 'x_reload.pdf'
    cmd_reload.large_icon = 'x_reload.pdf'
    cmd_reload.tooltip = 'Reload Plugin'

    menu = UI.menu('Plugins').add_submenu('RoundMe')
    menu.add_item(cmd_single_round)
    menu.add_item(cmd_multi_round)
    menu.add_item(cmd_reload)

    tb = UI::Toolbar.new('RoundMe')
    tb.add_item(cmd_single_round)
    tb.add_item(cmd_multi_round)
    tb.add_item(cmd_reload)
    tb.show

    file_loaded(__FILE__)
  end
end
