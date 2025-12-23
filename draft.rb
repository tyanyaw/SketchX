# frozen_string_literal: true

require 'sketchup'

module RoundMe
  class FilletTool
    def self.init
      puts '--- Sketchup Inspector ---'
      puts "1. Sketchup Version: #{Sketchup.version_number}"
      puts "2. Ruby Version : #{RUBY_VERSION}"
      puts "3. Inspect Active Model : #{Sketchup.active_model.inspect}"
      puts "4. Time : #{Time.now}"
      puts '--------------------------'
    end

    def self.run
      # 1. Get user input
      prompts = ['Radius: ', 'Segments: ']
      defaults = [10.0, 12]
      input = UI.inputbox(prompts, defaults, 'Round Corners Settings')
      return unless input

      radius = input[0].to_l # Converts string/number to Length object
      segments = input[1].to_i

      model = Sketchup.active_model
      selection = model.selection.grep(Sketchup::Edge)

      if selection.length < 2
        UI.messagebox('Please select at least 2 connected edges.')
        return
      end

      # 2. Start Logic
      commit_chain_round(selection, radius, segments)
    end

    # Helper method to find a common vertex between two edges
    def self.get_common_vertex(edge_a, edge_c)
      # Intersection of the two vertex arrays gives the shared vertex
      (edge_a.vertices & edge_c.vertices).first
    end

    def self.get_corner_data(edge_a, edge_c, radius)
      common_v = get_common_vertex(edge_a, edge_c)
      return nil unless common_v

      b_pos = common_v.position
      v_ba = (edge_a.other_vertex(common_v).position - b_pos).normalize
      v_bc = (edge_c.other_vertex(common_v).position - b_pos).normalize

      # 1. Calculate angle and tangent distance
      dot = v_ba.dot(v_bc).clamp(-1.0, 1.0)
      theta = Math.acos(dot)

      # Safety: don't draw if edges are collinear
      return nil if theta >= Math::PI || theta <= 0

      # 2. Tangent and Center distances
      t_dist = radius / Math.tan(theta / 2.0)
      h_dist = radius / Math.sin(theta / 2.0)

      # 3. Calculate points
      p1 = b_pos.offset(v_ba, t_dist)
      p2 = b_pos.offset(v_bc, t_dist)

      # Arc properties
      bisector = (v_ba + v_bc).normalize
      o_pos = b_pos.offset(bisector, h_dist)

      # 4. Vectors for Arc
      # xaxis must point from Center to P1
      # yaxis must point from Center towards P2 (perpendicular to xaxis)
      vec_o_p1 = (p1 - o_pos).normalize
      vec_o_p2 = (p2 - o_pos).normalize

      normal = (vec_o_p1 * vec_o_p2).normalize

      # The sweep angle is the supplement of the corner angle
      sweep_angle = Math::PI - theta

      { center: o_pos, xaxis: vec_o_p1, normal: normal,
        angle: sweep_angle, b_pos: b_pos }
    end

    def self.draw_arc_only(entities, edge_a, edge_c, radius, segments)
      data = get_corner_data(edge_a, edge_c, radius)
      return nil unless data

      # SketchUp auto-splits edges when arc is added at tangent points
      entities.add_arc(data[:center], data[:xaxis], data[:normal], radius, 0, data[:angle], segments)
      data[:b_pos]
    end

    def self.commit_chain_round(selection, radius, segments)
      model = Sketchup.active_model
      entities = model.active_entities

      # 1. Identify all unique shared vertices (corners)
      # We store them by position to avoid rounding the same corner twice
      corners = []

      selection.each_with_index do |e1, i|
        selection.each_with_index do |e2, j|
          next if i >= j # Avoid redundant pair checks

          common_v = get_common_vertex(e1, e2)
          if common_v
            # Store the pair and the shared vertex
            corners << { edge_a: e1, edge_c: e2, vertex: common_v }
          end
        end
      end

      model.start_operation('Round Corners', true)

      corner_positions = []
      corners.each do |c|
        # Draw the arc and collect the corner position (B) for deletion
        b_pos = draw_arc_only(entities, c[:edge_a], c[:edge_c], radius, segments)
        corner_positions << b_pos if b_pos
      end

      # 2. Delete "tangent edges" (leftover segments connected to original corners)
      to_delete = []
      entities.grep(Sketchup::Edge).each do |edge|
        # If either end of the edge is at an original corner position, it's a "tangent edge"
        to_delete << edge if corner_positions.any? { |pos| edge.start.position == pos || edge.end.position == pos }
      end

      entities.erase_entities(to_delete.uniq)
      model.commit_operation
    end
  end

  # Create Menu Item
  unless file_loaded?(__FILE__)
    self::FilletTool.init

    menu = UI.menu('Plugins')
    menu.add_item('RoundMe') { self::FilletTool.run }

    toolbar = UI::Toolbar.new 'RoundMe'

    tool_round = UI::Command.new('ROUND') do
      self::FilletTool.run
    end
    tool_round.tooltip = 'Round Corner'

    tool_reload = UI::Command.new('RELOAD') do
      load __FILE__
      Sketchup.active_model.select_tool(nil)
      UI.messagebox('Plugin Reloaded')
    end
    tool_reload.tooltip = 'Reload Plugin'

    toolbar.add_item tool_round
    toolbar.add_item tool_reload
    toolbar.show

    file_loaded(__FILE__)
  end
end

#################
#
#
# frozen_string_literal: true

require 'sketchup'
require 'extensions'

module RoundMe
  class FilletTool
    def self.info
      puts '--- RoundMe Diagnostics ---'
      puts "Sketchup Version: #{Sketchup.version}"
      puts "Ruby Version: #{RUBY_VERSION}"
      puts '---------------------------'
    end

    def initialize
      @radius = 50.mm
      @segments = 12
      @state = 0 # 0: Hover/Select, 1: Drag/VCB
      @ip = Sketchup::InputPoint.new
      @edge_a = @edge_b = nil
      @corner_vertex = nil
      @preview_data = nil
    end

    def activate
      update_ui
    end

    def update_ui
      # Multi-input VCB Support: Radius;Segments
      Sketchup.vcb_label = 'Radius; Segments'
      Sketchup.vcb_value = "#{@radius}; #{@segments}s"
      msg = @state == 0 ? 'Hover over corner. CLICK to select.' : 'MOVE to resize. CLICK to commit.'
      Sketchup.set_status_text(msg)
    end

    def onUserText(text, view)
      # Advanced VCB Parsing
      if text.include?(';')
        parts = text.split(';')
        parse_radius(parts[0])
        parse_segments(parts[1])
      elsif text.downcase.include?('s')
        parse_segments(text)
      else
        parse_radius(text)
      end

      if @state == 1 && @edge_a && @edge_b
        @preview_data = calc_round(@edge_a, @edge_b, @radius)
        finalize_rounding if @preview_data
      end
      view.invalidate
    rescue StandardError
      UI.beep
    end

    def parse_radius(t)
      @radius = t.to_l if t && !t.strip.empty?
    end

    def parse_segments(t)
      val = t.downcase.delete('s').to_i
      @segments = val if val > 2
    end

    def onMouseMove(flags, x, y, view)
      @ip.pick(view, x, y)
      if @state == 0
        handle_hover_selection(view)
      elsif @state == 1
        handle_drag_radius(view)
      end
      update_ui
      view.invalidate
    end

    def onLButtonDown(flags, x, y, view)
      if @state == 0 && @edge_a && @edge_b
        @state = 1
      elsif @state == 1
        finalize_rounding
      end
      update_ui
    end

    def handle_hover_selection(view)
      # 1. Reset selection data
      @edge_a = nil
      @edge_b = nil
      @corner_vertex = nil

      # 2. Get the edge under mouse
      picked_edge = @ip.edge
      return unless picked_edge

      # 3. Find closest vertex to cursor
      v_start = picked_edge.start
      v_end = picked_edge.end
      @corner_vertex = v_start.position.distance(@ip.position) < v_end.position.distance(@ip.position) ? v_start : v_end

      # 4. Find connected edge (Edge B)
      connected = @corner_vertex.edges.reject { |e| e == picked_edge }
      return unless connected.length > 0

      @edge_a = picked_edge
      @edge_b = connected.first

      # 5. IMPORTANT: Tell SketchUp to repaint the view
      # This triggers the 'draw' method which handles the blue lines.
      view.invalidate
    end

    def handle_drag_radius(view)
      raw_dist = @corner_vertex.position.distance(@ip.position)
      # MAX RADIUS CONSTRAINT: i don't want it out bound
      max_allowed = [@edge_a.length, @edge_b.length].min
      @radius = [raw_dist, max_allowed].min
      @radius = 0.1.mm if @radius < 0.1.mm

      @preview_data = calc_round(@edge_a, @edge_b, @radius)
    end

    def calc_round(edge_a, edge_c, radius)
      line_a = edge_a.line
      line_c = edge_c.line
      b_pos = Geom.intersect_line_line(line_a, line_c)
      return nil unless b_pos

      # Vectors pointing away from the corner
      a_pos = edge_a.other_vertex(@corner_vertex).position
      c_pos = edge_c.other_vertex(@corner_vertex).position
      vector_ba = (a_pos - b_pos).normalize
      vector_bc = (c_pos - b_pos).normalize

      b_angle = vector_ba.angle_between(vector_bc)
      return nil if [0, Math::PI].include?(b_angle)

      o_angle = Math::PI - b_angle
      dist_to_center = radius / Math.sin(b_angle / 2.0)

      # Correcting Inversion: Center O must be on the interior
      bisector = (vector_ba + vector_bc).normalize
      o_pos = b_pos.offset(bisector, dist_to_center)

      # FIX: Use .length = instead of .length(argument)
      normal = vector_ba.cross(vector_bc).normalize

      vector_p1 = vector_ba.clone
      vector_p1.length = radius / Math.tan(b_angle / 2.0)
      p1_pos = b_pos.offset(vector_p1)

      vector_p2 = vector_bc.clone
      vector_p2.length = radius / Math.tan(b_angle / 2.0)
      p2_pos = b_pos.offset(vector_p2)

      xaxis = (p1_pos - o_pos).normalize
      yaxis = (p2_pos - o_pos).normalize

      # Final orientation check
      test_tr = Geom::Transformation.rotation(o_pos, normal, o_angle)
      test_vec = xaxis.transform(test_tr)
      test_vec.length = radius
      test_pt = o_pos + test_vec

      normal.reverse! if test_pt.distance(p2_pos) > 0.001.mm

      {
        xaxis: xaxis,
        yaxis: yaxis,
        normal: normal,
        radius: radius,
        vector_ba: vector_ba,
        vector_bc: vector_bc,
        vector_p1: vector_p1,
        vector_p2: vector_p2,
        a_pos: a_pos,
        b_pos: b_pos,
        c_pos: c_pos,
        o_pos: o_pos,
        p1_pos: p1_pos,
        p2_pos: p2_pos,
        b_angle: b_angle,
        o_angle: o_angle,
        dist_to_center: dist_to_center
      }
    end

    def draw(view)
      # 1. Highlight selected edges
      if @edge_a && @edge_a.valid?
        view.line_width = 3
        view.drawing_color = 'blue'
        view.draw(GL_LINES, @edge_a.start.position, @edge_a.end.position)
      end

      if @edge_b && @edge_b.valid?
        view.line_width = 3
        view.drawing_color = 'blue'
        view.draw(GL_LINES, @edge_b.start.position, @edge_b.end.position)
      end

      # 2. Draw the red corner vertex if it exists
      if @corner_vertex
        view.line_width = 3
        view.draw_points([@corner_vertex.position], 20, 2, 'red')
      end

      # 3. Guard clause for the arc preview ONLY
      return unless @preview_data

      # 4. Draw the orange arc preview
      points = []
      angle_step = @preview_data[:o_angle] / @segments
      (0..@segments).each do |i|
        tr = Geom::Transformation.rotation(@preview_data[:o_pos], @preview_data[:normal], i * angle_step)
        vec = @preview_data[:xaxis].transform(tr)
        vec.length = @radius
        points << @preview_data[:o_pos] + vec
      end
      view.line_width = 2
      view.drawing_color = 'orange'
      view.draw(GL_LINE_STRIP, points)

      # Draw dashed radius lines (Gray)
      p1_line = @preview_data[:p1_pos], @preview_data[:o_pos]
      p2_line = @preview_data[:p2_pos], @preview_data[:o_pos]
      view.line_width = 1
      view.drawing_color = 'gray'
      view.line_stipple = '.' # Use '.' for a clean dotted look
      view.draw(GL_LINES, p1_line)
      view.draw(GL_LINES, p2_line)
      view.line_stipple = '' # Reset stipple so other lines aren't affected

      # 5. DRAW SMALL GRAY DOT FOR CENTER O
      # Change: Wrapped in [ ] to make it an array of points
      view.draw_points([@preview_data[:o_pos]], 10, 2, 'gray')
    end

    def finalize_rounding
      return unless @preview_data

      model = Sketchup.active_model
      model.start_operation('Round Corner', true)
      model.active_entities.add_arc(@preview_data[:o_pos], @preview_data[:xaxis], @preview_data[:normal],
                                    @preview_data[:radius], 0.0, @preview_data[:o_angle])
      model.commit_operation
      @state = 0
      @preview_data = nil
    end
  end

  # ----------------------------------------------------------------------------
  # LOADER
  # ----------------------------------------------------------------------------
  unless file_loaded?(__FILE__)
    cmd_round = UI::Command.new('Round Corner') do
      Sketchup.active_model.select_tool(RoundMe::FilletTool.new)
    end
    cmd_round.small_icon = 'x_round.pdf'
    cmd_round.large_icon = 'x_round.pdf'
    cmd_round.tooltip = 'Round Corner'

    cmd_reload = UI::Command.new('Reload') do
      load __FILE__
      RoundMe::FilletTool.info
      puts 'RoundMe Reloaded'
    end

    cmd_reload.small_icon = 'x_reload.pdf'
    cmd_reload.large_icon = 'x_reload.pdf'

    menu = UI.menu('Plugins').add_submenu('RoundMe')
    menu.add_item(cmd_round)
    menu.add_item(cmd_reload)

    toolbar = UI::Toolbar.new 'RoundMe'
    toolbar.add_item cmd_round
    toolbar.add_item cmd_reload
    toolbar.show

    file_loaded(__FILE__)
  end
end
