package body Nazar.Controllers.Draw is

   -------------------
   -- Start_Draw --
   -------------------

   procedure Start_Draw
     (Controller : in out Root_Draw_Controller;
      Model      : not null access Controller_Model;
      View       : not null access Controller_View)
   is
   begin
      Controller.Model := Model_Access (Model);
      Controller.View  := View_Access (View);
      View.Show;
   end Start_Draw;

end Nazar.Controllers.Draw;
