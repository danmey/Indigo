module Mouse = struct
  type button = Left | Right | Mid
  and mouse = { pos : Point.t; button : button }

  module Press = struct
    type t = { mouse : mouse; time_stamp:Timestamp.t }
  end
end
