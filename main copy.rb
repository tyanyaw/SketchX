# frozen_string_literal: true

require 'sketchup'

module RoundMe
  class FilletTool
    def initialize
      @model = Sketchup.active_model
      @selection = @model.selection.grep(Sketchup::Edge)
      @preview_points = []
    end

    def self.run
      tool = new
      return UI.messagebox('Select at least 2 connected edges.') if tool.invalid_selection?

      tool.show_dialog
    end

    def invalid_selection?
      @selection.length < 2
    end

    def show_dialog
      prompts = ['Radius: ', 'Segments: ']
      defaults = [10.0.to_l, 12]

      # The preview logic needs to run while the box is open or via a Tool class
      # For a standard UI.inputbox, we calculate the corners first
      corners = identify_corners

      input = UI.inputbox(prompts, defaults, 'Round Corners Settings')

      if input
        commit_round(corners, input[0].to_l, input[1].to_i)
      else
        @model.select_tool(nil) # Cancel
      end
    end

    private

    # 1. FIX: Identify corners by position, not by volatile edge objects
    def identify_corners
      corners = []
      edges = @selection.to_a

      edges.each_with_index do |e1, i|
        edges.each_with_index do |e2, j|
          next if i >= j

          common_v = (e1.vertices & e2.vertices).first
          next unless common_v

          # Store geometric vectors and original vertex position
          b_pos = common_v.position
          v_ba = (e1.other_vertex(common_v).position - b_pos).normalize
          v_bc = (e2.other_vertex(common_v).position - b_pos).normalize

          corners << { b: b_pos, v1: v_ba, v2: v_bc }
        end
      end
      corners
    end

    def calculate_arc_points(corner, radius, segments)
      # Geometric math from your core logic
      dot = corner[:v1].dot(corner[:v2]).clamp(-1.0, 1.0)
      theta = Math.acos(dot)
      return nil if theta >= Math::PI || theta <= 0

      t_dist = radius / Math.tan(theta / 2.0)
      h_dist = radius / Math.sin(theta / 2.0)

      p1 = corner[:b].offset(corner[:v1], t_dist)
      p2 = corner[:b].offset(corner[:v2], t_dist)
      bisector = (corner[:v1] + corner[:v2]).normalize
      center = corner[:b].offset(bisector, h_dist)

      # Generate points for the arc
      vec_o_p1 = (p1 - center).normalize
      vec_o_p2 = (p2 - center).normalize
      normal = (vec_o_p1 * vec_o_p2).normalize

      points = []
      sweep = Math::PI - theta
      (0..segments).each do |i|
        angle = (sweep / segments) * i
        tr = Geom::Transformation.rotation(center, normal, angle)
        points << p1.transform(tr)
      end
      { points: points, b: corner[:b] }
    end

    def commit_round(corners, radius, segments)
      @model.start_operation('Round Corners', true)
      entities = @model.active_entities
      corner_positions = []

      # Draw arcs first to trigger auto-splitting
      corners.each do |c|
        data = calculate_arc_points(c, radius, segments)
        next unless data

        # Draw arc using native method to ensure edge splitting
        # We find vectors for the native call
        vec_center_p1 = (data[:points].first - data[:points].first) # dummy
        # Re-using your logic for add_arc
        v1 = (data[:points].first - data[:points].first.offset(data[:points].first - data[:points].last, 1)) # dummy simplified

        # Drawing the polyline/arc
        entities.add_curve(data[:points])
        corner_positions << data[:b]
      end

      # 2. Cleanup: Delete edges connected to original corner points
      to_delete = entities.grep(Sketchup::Edge).select do |edge|
        corner_positions.any? { |pos| edge.start.position == pos || edge.end.position == pos }
      end

      entities.erase_entities(to_delete)
      @model.commit_operation
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
