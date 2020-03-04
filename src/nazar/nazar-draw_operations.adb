package body Nazar.Draw_Operations is

   --------------------
   -- World_Position --
   --------------------

   function World_Position
     (World_X, World_Y : Nazar_Float) return Draw_Position
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "World_Position unimplemented");
      return raise Program_Error with "Unimplemented function World_Position";
   end World_Position;

   ---------------------
   -- Screen_Position --
   ---------------------

   function Screen_Position
     (Screen_X, Screen_Y : Nazar_Float) return Draw_Position
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Screen_Position unimplemented");
      return raise Program_Error with "Unimplemented function Screen_Position";
   end Screen_Position;

   -------------------------
   -- Get_Screen_Position --
   -------------------------

   procedure Get_Screen_Position
     (Context : Draw_Context; X, Y : out Nazar_Float)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Get_Screen_Position unimplemented");
      raise Program_Error with "Unimplemented procedure Get_Screen_Position";
   end Get_Screen_Position;

   ----------------
   -- Set_Target --
   ----------------

   procedure Set_Target
     (Context : in out Draw_Context; Width : Nazar_Float; Height : Nazar_Float)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Set_Target unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Target";
   end Set_Target;

   ------------------
   -- Set_Viewport --
   ------------------

   procedure Set_Viewport (Context : in out Draw_Context; Viewport : Rectangle)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Set_Viewport unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Viewport";
   end Set_Viewport;

   --------------
   -- Get_Fill --
   --------------

   function Get_Fill (Context : Draw_Context) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get_Fill unimplemented");
      return raise Program_Error with "Unimplemented function Get_Fill";
   end Get_Fill;

   --------------------
   -- Color_Property --
   --------------------

   function Color_Property
     (Color : Nazar.Colors.Nazar_Color) return Draw_Property
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Color_Property unimplemented");
      return raise Program_Error with "Unimplemented function Color_Property";
   end Color_Property;

   -------------------
   -- Fill_Property --
   -------------------

   function Fill_Property (Fill : Boolean) return Draw_Property is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Fill_Property unimplemented");
      return raise Program_Error with "Unimplemented function Fill_Property";
   end Fill_Property;

   ----------
   -- Move --
   ----------

   function Move (To : Draw_Position; Paint : Boolean) return Draw_Operation is
   begin
      pragma Compile_Time_Warning (Standard.True, "Move unimplemented");
      return raise Program_Error with "Unimplemented function Move";
   end Move;

   ---------
   -- Arc --
   ---------

   function Arc
     (Radius    : Nazar_Float; Start_Angle : Nazar.Trigonometry.Angle;
      End_Angle : Nazar.Trigonometry.Angle) return Draw_Operation
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Arc unimplemented");
      return raise Program_Error with "Unimplemented function Arc";
   end Arc;

   ------------------
   -- Set_Property --
   ------------------

   function Set_Property (Property : Draw_Property) return Draw_Operation is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Set_Property unimplemented");
      return raise Program_Error with "Unimplemented function Set_Property";
   end Set_Property;

   ----------------
   -- Save_State --
   ----------------

   function Save_State return Draw_Operation is
   begin
      pragma Compile_Time_Warning (Standard.True, "Save_State unimplemented");
      return raise Program_Error with "Unimplemented function Save_State";
   end Save_State;

   -------------------
   -- Restore_State --
   -------------------

   function Restore_State return Draw_Operation is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Restore_State unimplemented");
      return raise Program_Error with "Unimplemented function Restore_State";
   end Restore_State;

   ----------------
   -- Save_State --
   ----------------

   procedure Save_State (Render : in out Root_Render_Type) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Save_State unimplemented");
      raise Program_Error with "Unimplemented procedure Save_State";
   end Save_State;

   -------------------
   -- Restore_State --
   -------------------

   procedure Restore_State (Render : in out Root_Render_Type) is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Restore_State unimplemented");
      raise Program_Error with "Unimplemented procedure Restore_State";
   end Restore_State;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Render    : in out Root_Render_Type'Class; Context : Draw_Context;
      Operation :        Draw_Operation)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Draw unimplemented");
      raise Program_Error with "Unimplemented procedure Draw";
   end Draw;

end Nazar.Draw_Operations;
