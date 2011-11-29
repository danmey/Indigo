module Mouse = struct
  type button = Left | Right | Mid
  and mouse = { pos : Pos.t; button : button }

  module Click = struct 
    type t = { mouse : mouse; time_stamp:Timestamp.t }
  end

  module Press = struct 
    type t = { mouse : mouse; time_stamp:Timestamp.t }
  end
end
