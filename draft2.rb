## WORK single vertex. face auto, need to edited on face
#
# frozen_string_literal: true

require 'sketchup'
require 'extensions'

module RoundMe
  class FilletTool
    # --------------------------------------------------------------------------
    # 1. CLASS METHODS (The "Math Engine" & "Manager")
    # --------------------------------------------------------------------------

    # A. THE MANAGER: Handles Face loops or Edge selections
    def self.loop_selection(selection, radius, segments)
      model = Sketchup.active_model
      corners_to_process = []

      # 1. Analyze Selection
      if selection.is_a?(Sketchup::Selection) || selection.is_a?(Array)
        items = selection.grep(Sketchup::Face)
        if items.empty?
          # Fallback: Selected Edges (Requires exactly 2 edges logic for now)
          edges = selection.grep(Sketchup::Edge)
          return UI.messagebox('Select a Face or 2 intersecting Edges') if edges.length < 2

          # Try to find a common vertex for 2 edges
          common_vtx = edges[0].vertices.find { |v| edges[1].vertices.include?(v) }
          data = extract_data(edges[0], edges[1], common_vtx)
          corners_to_process << data if data[:valid]
        else
          # Face Mode: Iterate all vertices
          items.each do |face|
            face.vertices.each do |vtx|
              edges = vtx.edges.select { |e| face.edges.include?(e) }
              next unless edges.length == 2

              data = extract_data(edges[0], edges[1], vtx)
              corners_to_process << data if data[:valid]
            end
          end
        end
      end

      # 2. Execution Loop
      model.start_operation('Round Selection', true)
      corners_to_process.each do |corner|
        # Recalculate pure math based on validated points
        math_result = calc_round(corner[:pos_a], corner[:pos_b], corner[:pos_c], radius)
        draw_2d_round(model, math_result, segments) if math_result
      end
      model.commit_operation
    end

    # B. THE CLEANER: Extracts & Validates Data (No drawing)
    def self.extract_data(edge_a, edge_c, vtx = nil)
      # Identify shared corner (B)
      if vtx
        pos_b = vtx.position
      else
        pos_b = Geom.intersect_line_line(edge_a.line, edge_c.line)
        return { valid: false, error: 'Parallel' } unless pos_b
      end

      # Identify Far Points (A & C)
      pos_a = edge_a.other_vertex(vtx || edge_a.start).position
      pos_c = edge_c.other_vertex(vtx || edge_c.start).position

      # Safety: Calculate Max Radius allowed
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

    # C. THE MATH ENGINE: Pure Geometry (Shadow Math)
    def self.calc_round(pos_a, pos_b, pos_c, radius)
      vector_ba = (pos_a - pos_b).normalize
      vector_bc = (pos_c - pos_b).normalize

      b_angle = vector_ba.angle_between(vector_bc)
      # Guard: Collinear or Zero angle
      return nil if b_angle < 0.01 || b_angle > (Math::PI - 0.01)

      o_angle = Math::PI - b_angle
      dist_to_center = radius / Math.sin(b_angle / 2.0)

      # Bisector
      bisector = (vector_ba + vector_bc).normalize
      pos_o = pos_b.offset(bisector, dist_to_center)

      # Tangent Points & Normal
      tangent_len = radius / Math.tan(b_angle / 2.0)
      normal = vector_ba.cross(vector_bc).normalize

      # Fix Normal Inversion
      # Test a point on the hypothetical arc
      pos_p1 = pos_b.offset(vector_ba, tangent_len)
      pos_p2 = pos_b.offset(vector_bc, tangent_len)
      vector_p1 = vector_ba.clone
      vector_p1.length = tangent_len
      vector_p2 = vector_bc.clone
      vector_p2.length = tangent_len

      xaxis = (pos_p1 - pos_o).normalize
      yaxis = (pos_p2 - pos_o).normalize

      # Orientation check
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

    # D. THE DRAWER: Commits Geometry
    def self.draw_2d_round(model, data, segments)
      # Create Arc
      model.active_entities.add_arc(
        data[:pos_o], data[:xaxis], data[:normal],
        data[:radius], 0.0, data[:o_angle], segments
      )
      # Optional: Add trim logic here in future
    end

    # E. FUTURE PLACEHOLDER
    def self.draw_3d_round(selection)
      # TODO: Implement 3-edge corner sphere/patch logic
      puts '3D Rounding not yet implemented'
    end

    # --------------------------------------------------------------------------
    # 2. INSTANCE METHODS (The Interactive Tool)
    # --------------------------------------------------------------------------
    def initialize
      @radius = 50.mm
      @segments = 12
      @state = 0 # 0: Hover, 1: Drag
      @ip = Sketchup::InputPoint.new

      # Data Holders
      @edge_a = nil
      @edge_b = nil
      @corner_data = nil # Holds result from extract_data
      @preview_data = nil # Holds result from calc_round
    end

    def activate
      update_ui
      Sketchup.active_model.selection.clear
    end

    def update_ui
      Sketchup.vcb_label = 'Radius; Segments'
      Sketchup.vcb_value = "#{@radius}; #{@segments}s"
      msg = @state == 0 ? 'Hover Corner' : 'Drag to Resize'
      Sketchup.set_status_text(msg)
    end

    # INPUT MODE: HYBRID (Mouse Move handles both Hover & Drag)
    def onMouseMove(flags, x, y, view)
      @ip.pick(view, x, y)

      if @state == 0
        # Mode A: Hover / Select
        handle_hover(view)
      elsif @state == 1
        # Mode B: Drag / Adjust
        handle_drag(view)
      end

      update_ui
      view.invalidate
    end

    def onLButtonDown(flags, x, y, view)
      if @state == 0 && @corner_data && @corner_data[:valid]
        @state = 1 # Lock selection, start dragging
      elsif @state == 1
        finalize_tool # Commit
      end
    end

    def onUserText(text, view)
      # VCB Parsing
      if text.include?(';')
        r_str, s_str = text.split(';')
        @radius = r_str.to_l
        @segments = s_str.to_i
      elsif text.end_with?('s')
        @segments = text.to_i
      else
        @radius = text.to_l
      end

      # If dragging, force update preview
      if @state == 1 && @corner_data
        @preview_data = FilletTool.calc_round(@corner_data[:pos_a], @corner_data[:pos_b], @corner_data[:pos_c],
                                              @radius)
        finalize_tool # Auto-commit on VCB enter
      end
    rescue StandardError
      UI.beep
    end

    # --- Helpers for Tool ---

    def handle_hover(view)
      @edge_a = nil
      @edge_b = nil
      @corner_data = nil

      picked = @ip.edge
      return unless picked

      # Find connected edge
      v_near = picked.start.position.distance(@ip.position) < picked.end.position.distance(@ip.position) ? picked.start : picked.end
      connected = v_near.edges.reject { |e| e == picked }.first
      return unless connected

      # Extract Data (The Cleaner)
      data = FilletTool.extract_data(picked, connected, v_near)

      return unless data[:valid]

      @edge_a = picked
      @edge_b = connected
      @corner_data = data
      # Calculate Pivot O for preview
      @preview_data = FilletTool.calc_round(data[:pos_a], data[:pos_b], data[:pos_c], @radius)
    end

    def handle_drag(view)
      # Update radius based on mouse distance from corner
      dist = @corner_data[:pos_b].distance(@ip.position)

      # Clamp to max limit found in extraction
      limit = @corner_data[:max_limit]
      @radius = [dist, limit].min
      @radius = 1.mm if @radius < 1.mm

      # Recalculate (Math Engine)
      @preview_data = FilletTool.calc_round(@corner_data[:pos_a], @corner_data[:pos_b], @corner_data[:pos_c], @radius)
    end

    def finalize_tool
      return unless @preview_data

      FilletTool.draw_2d_round(Sketchup.active_model, @preview_data, @segments)

      # Reset
      @state = 0
      @preview_data = nil
      @corner_data = nil
      @edge_a = nil
      @edge_b = nil
    end

    def draw(view)
      # 1. Selection Highlights (Blue)
      if @edge_a && @edge_a.valid?
        view.line_width = 3
        view.drawing_color = 'blue'
        view.draw(GL_LINES, @edge_a.start.position, @edge_a.end.position)
        view.draw(GL_LINES, @edge_b.start.position, @edge_b.end.position)
      end

      # 2. Pivot Point (Gray Dot) & Red Corner
      view.draw_points([@corner_data[:pos_b]], 10, 2, 'red') if @corner_data

      # 3. Preview Arc (Orange)
      return unless @preview_data

      view.draw_points([@preview_data[:pos_o]], 8, 2, 'gray')

      points = []
      step = @preview_data[:o_angle] / @segments
      (0..@segments).each do |i|
        tr = Geom::Transformation.rotation(@preview_data[:pos_o], @preview_data[:normal], i * step)
        vec = @preview_data[:xaxis].transform(tr)
        vec.length = @radius

        pt = @preview_data[:pos_o] + vec
        points << pt
      end

      view.line_width = 2
      view.drawing_color = 'orange'
      view.draw(GL_LINE_STRIP, points)
    end
  end

  # ----------------------------------------------------------------------------
  # 3. LOADER & UI
  # ----------------------------------------------------------------------------
  unless file_loaded?(__FILE__)

    # Tool Command
    cmd_tool = UI::Command.new('Round Corner') do
      Sketchup.active_model.select_tool(RoundMe::FilletTool.new)
    end
    cmd_tool.small_icon = 'x_round.pdf'
    cmd_tool.large_icon = 'x_round.pdf'
    cmd_tool.tooltip = 'Interactive Round Tool'

    # Face Command (Uses Loop Selection)
    cmd_face = UI::Command.new('Round Face') do
      sel = Sketchup.active_model.selection
      Sketchup.active_model.select_tool(RoundMe::FilletTool.new)
      # Use defaults or prompt in future
      # FilletTool.loop_selection(sel, 50.mm, 12)
    end
    cmd_face.small_icon = 'x_round.pdf'
    cmd_face.large_icon = 'x_round.pdf'
    cmd_face.tooltip = 'Round Selected Face (Default 50mm)'

    # Reload Command
    cmd_reload = UI::Command.new('Reload') do
      load __FILE__
      puts 'RoundMe Reloaded'
    end
    cmd_reload.small_icon = 'x_reload.pdf'
    cmd_reload.large_icon = 'x_reload.pdf'
    cmd_reload.tooltip = 'Reload Plugin'

    # Menu
    menu = UI.menu('Plugins').add_submenu('RoundMe')
    menu.add_item(cmd_tool)
    menu.add_item(cmd_face)
    menu.add_item(cmd_reload)

    # Toolbar
    tb = UI::Toolbar.new('RoundMe')
    tb.add_item(cmd_tool)
    tb.add_item(cmd_face)
    tb.add_item(cmd_reload)
    tb.show

    file_loaded(__FILE__)
  end
end # Module
