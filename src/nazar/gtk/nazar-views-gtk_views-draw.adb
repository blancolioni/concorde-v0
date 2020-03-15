with Gdk.Event;
with Gdk.Window;

with Nazar.Colors;
with Nazar.Draw_Operations;

package body Nazar.Views.Gtk_Views.Draw is

   function From_Object is
     new Nazar.Views.Gtk_Views.From_Gtk_Object
       (Nazar_Gtk_Draw_View_Record, Nazar_Gtk_Draw_View);

   function Configure_Handler
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Configure)
      return Boolean;

   function Draw_Handler
     (Self  : access Glib.Object.GObject_Record'Class;
      Cr    : Cairo.Cairo_Context)
      return Boolean;

   procedure Clear
     (Surface : Cairo.Cairo_Surface;
      Color   : Nazar.Colors.Nazar_Color);

   type Cairo_Render_Type is
     new Nazar.Draw_Operations.Root_Render_Type with
      record
         Cr      : Cairo.Cairo_Context;
         X, Y    : Glib.Gdouble := 0.0;
      end record;

   overriding procedure Move_To
     (Render : in out Cairo_Render_Type;
      X, Y   : Nazar_Float);

   overriding procedure Line_To
     (Render : in out Cairo_Render_Type;
      X, Y   : Nazar_Float);

   overriding procedure Arc
     (Render        : in out Cairo_Render_Type;
      Radius        : Nazar_Float;
      Start_Radians : Nazar_Float;
      End_Radians   : Nazar_Float);

   overriding procedure Render_Current
     (Render   : in out Cairo_Render_Type;
      Fill     : Boolean;
      Preserve : Boolean);

   overriding procedure Set_Color
     (Render : in out Cairo_Render_Type;
      Color  : Nazar.Colors.Nazar_Color);

   overriding procedure Save_State
     (Render : in out Cairo_Render_Type);

   overriding procedure Restore_State
     (Render : in out Cairo_Render_Type);

   ---------
   -- Arc --
   ---------

   overriding procedure Arc
     (Render        : in out Cairo_Render_Type;
      Radius        : Nazar_Float;
      Start_Radians : Nazar_Float;
      End_Radians   : Nazar_Float)
   is
   begin
      Cairo.Arc (Render.Cr, Render.X, Render.Y,
                 Glib.Gdouble (Radius),
                 Glib.Gdouble (Start_Radians),
                 Glib.Gdouble (End_Radians));
   end Arc;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Surface : Cairo.Cairo_Surface;
      Color   : Nazar.Colors.Nazar_Color)
   is
      Cr : constant Cairo.Cairo_Context := Cairo.Create (Surface);
   begin
      Cairo.Set_Source_Rgb
        (Cr,
         Glib.Gdouble (Color.Red),
         Glib.Gdouble (Color.Green),
         Glib.Gdouble (Color.Blue));
      Cairo.Paint (Cr);
      Cairo.Destroy (Cr);
   end Clear;

   -----------------------
   -- Configure_Handler --
   -----------------------

   function Configure_Handler
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Configure)
      return Boolean
   is
      use Glib;
      use type Cairo.Cairo_Surface;
      use type Nazar.Models.Nazar_Model;
      View   : constant Nazar_Gtk_Draw_View := From_Object (Self);
      Width  : constant Gint := Event.Width;
      Height : constant Gint := Event.Height;
   begin

      if View.Surface /= Cairo.Null_Surface then
         Cairo.Surface_Destroy (View.Surface);
      end if;

      View.Surface :=
        Gdk.Window.Create_Similar_Surface
           (View.Draw_Area.Get_Window,
            Cairo.Cairo_Content_Color,
            View.Draw_Area.Get_Allocated_Width,
            View.Draw_Area.Get_Allocated_Height);

      if View.Model = null then
         Clear (View.Surface,
                (0.6, 0.45, 0.82, 1.0));
      else
         declare
            Context : Nazar.Draw_Operations.Draw_Context;
            Render  : Cairo_Render_Type;
         begin
            Clear (View.Surface, View.Draw_Model.Background_Color);
            Render.Cr := Cairo.Create (View.Surface);
            Nazar.Draw_Operations.Set_Target
              (Context, Nazar_Float (Width), Nazar_Float (Height));
            Nazar.Draw_Operations.Set_Viewport (Context, View.Viewport);
            View.Draw_Model.Render (Context, Render);
            Cairo.Destroy (Render.Cr);
         end;
      end if;

      return True;

   end Configure_Handler;

   ------------------
   -- Draw_Handler --
   ------------------

   function Draw_Handler
     (Self  : access Glib.Object.GObject_Record'Class;
      Cr    : Cairo.Cairo_Context)
      return Boolean
   is
      View   : constant Nazar_Gtk_Draw_View := From_Object (Self);
   begin
      Cairo.Set_Source_Surface (Cr, View.Surface, 0.0, 0.0);
      Cairo.Paint (Cr);
      return True;
   end Draw_Handler;

   -------------
   -- Line_To --
   -------------

   overriding procedure Line_To
     (Render : in out Cairo_Render_Type;
      X, Y   : Nazar_Float)
   is
   begin
      Render.X := Glib.Gdouble (X);
      Render.Y := Glib.Gdouble (Y);
      Cairo.Line_To (Render.Cr, Render.X, Render.Y);
   end Line_To;

   -------------
   -- Move_To --
   -------------

   overriding procedure Move_To
     (Render : in out Cairo_Render_Type;
      X, Y   : Nazar_Float)
   is
   begin
      Render.X := Glib.Gdouble (X);
      Render.Y := Glib.Gdouble (Y);
      Cairo.Move_To (Render.Cr, Render.X, Render.Y);
   end Move_To;

   --------------------------------
   -- Nazar_Gtk_Draw_View_Create --
   --------------------------------

   function Nazar_Gtk_Draw_View_Create
     return Nazar_View
   is
      View : constant Nazar_Gtk_Draw_View :=
        new Nazar_Gtk_Draw_View_Record;
   begin
      View.Draw_Area := Gtk.Drawing_Area.Gtk_Drawing_Area_New;
      View.Initialize (View.Draw_Area);
      View.Draw_Area.On_Configure_Event
        (Configure_Handler'Access, View.Object);
      View.Draw_Area.On_Draw
        (Draw_Handler'Access, View.Object);
      return Nazar_View (View);
   end Nazar_Gtk_Draw_View_Create;

   -----------------------------
   -- Nazar_Gtk_Draw_View_New --
   -----------------------------

   function Nazar_Gtk_Draw_View_New
     (Model : not null access Nazar.Models.Draw.Root_Draw_Model'Class)
      return Nazar_Gtk_Draw_View
   is
      View : constant Nazar_Gtk_Draw_View :=
        Nazar_Gtk_Draw_View (Nazar_Gtk_Draw_View_Create);
   begin
      View.Viewport := Model.Bounding_Box;
      View.Set_Model (Model);
      return View;
   end Nazar_Gtk_Draw_View_New;

   --------------------
   -- Render_Current --
   --------------------

   overriding procedure Render_Current
     (Render   : in out Cairo_Render_Type;
      Fill     : Boolean;
      Preserve : Boolean)
   is
   begin
      if Fill then
         if Preserve then
            Cairo.Fill_Preserve (Render.Cr);
         else
            Cairo.Fill (Render.Cr);
         end if;
      else
         if Preserve then
            Cairo.Stroke_Preserve (Render.Cr);
         else
            Cairo.Stroke (Render.Cr);
         end if;
      end if;
   end Render_Current;

   -------------------
   -- Restore_State --
   -------------------

   overriding procedure Restore_State
     (Render : in out Cairo_Render_Type)
   is
   begin
      Nazar.Draw_Operations.Root_Render_Type (Render).Restore_State;
      Cairo.Restore (Render.Cr);
   end Restore_State;

   ----------------
   -- Save_State --
   ----------------

   overriding procedure Save_State
     (Render : in out Cairo_Render_Type)
   is
   begin
      Nazar.Draw_Operations.Root_Render_Type (Render).Save_State;
      Cairo.Save (Render.Cr);
   end Save_State;

   ---------------
   -- Set_Color --
   ---------------

   overriding procedure Set_Color
     (Render : in out Cairo_Render_Type;
      Color  : Nazar.Colors.Nazar_Color)
   is
   begin
      Cairo.Set_Source_Rgba
        (Render.Cr,
         Glib.Gdouble (Color.Red),
         Glib.Gdouble (Color.Green),
         Glib.Gdouble (Color.Blue),
         Glib.Gdouble (Color.Alpha));
   end Set_Color;

   ---------------
   -- Set_Model --
   ---------------

   overriding procedure Set_Model
     (View  : not null access Nazar_Gtk_Draw_View_Record;
      Model : not null access Nazar.Models.Nazar_Model_Record'Class)
   is
      Bounding_Box : constant Rectangle :=
        Nazar.Models.Draw.Nazar_Draw_Model (Model).Bounding_Box;
   begin
      View.Viewport := Bounding_Box;
      Nazar.Views.Gtk_Views.Nazar_Gtk_View_Record (View.all)
        .Set_Model (Model);
   end Set_Model;

   -----------------------
   -- Update_From_Model --
   -----------------------

   overriding procedure Update_From_Model
     (View : in out Nazar_Gtk_Draw_View_Record)
   is
   begin
      null;
   end Update_From_Model;

end Nazar.Views.Gtk_Views.Draw;
