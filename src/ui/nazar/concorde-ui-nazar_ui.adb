with Ada.Text_IO;

with Nazar.Views.Console;
with Nazar.Views.Draw;

with Nazar.Controllers.Console;
with Nazar.Controllers.Draw;

with Concorde.UI.Models.Console;
with Concorde.UI.Models.Galaxy;
with Concorde.UI.Models.Market;

with Concorde.Paths;

with Concorde.Db.Faction;
with Concorde.Handles.Faction;

with Concorde.Db.Market;
with Concorde.Handles.Market;

with Concorde.UI.Models.Current_Cash;
with Concorde.UI.Models.Current_Date;

package body Concorde.UI.Nazar_UI is

   type Root_Nazar_UI is
     new UI_Interface with
      record
         Top     : Nazar.Views.Nazar_View;
         Galaxy  : Nazar.Controllers.Draw.Nazar_Draw_Controller_Record;
         Console : Nazar.Controllers.Console.Nazar_Console_Controller_Record;
      end record;

   overriding procedure Start (UI : in out Root_Nazar_UI);

   overriding procedure Stop (UI : in out Root_Nazar_UI;
                              Message : String);

   type Nazar_UI_Access is access all Root_Nazar_UI'Class;

   ------------------
   -- Get_Nazar_UI --
   ------------------

   function Get_Nazar_UI
     (Creator : Nazar.Builder.Nazar_Creator_Interface'Class) return UI_Type
   is
      Result : constant Nazar_UI_Access := new Root_Nazar_UI;
      Builder : constant Nazar.Builder.Nazar_Builder :=
        Nazar.Builder.Nazar_Builder_New
          (Creator     => Creator,
           Config_Path => Concorde.Paths.Config_File ("ui/concorde.config"));
      Faction : constant Concorde.Handles.Faction.Faction_Handle :=
                  Concorde.Handles.Faction.Get
                    (Concorde.Db.Faction.First_Reference_By_Top_Record
                       (Concorde.Db.R_Faction));
   begin
      Result.Top := Builder.Get_View ("Concorde");
      Result.Console.Start_Console
        (Model =>
           Models.Console.Console_Model
             (Default_Scope => "/home"),
         View  =>
           Nazar.Views.Console.Nazar_Console_View
             (Builder.Get_View ("console")));
      Result.Galaxy.Start_Draw
        (Model =>
           Models.Galaxy.Galaxy_Model (Faction),
         View  =>
           Nazar.Views.Draw.Nazar_Draw_View
             (Builder.Get_View ("galaxy")));

      Builder.Get_View ("faction-label").Set_Property ("text", Faction.Name);
      Builder.Get_View ("date-label").Set_Model
        (Concorde.UI.Models.Current_Date.Current_Date_Model_New);
      Builder.Get_View ("cash-label").Set_Model
        (Concorde.UI.Models.Current_Cash.Current_Cash_Model_New (Faction));

      Builder.Get_View ("market").Set_Model
        (Concorde.UI.Models.Market.Market_Model
           (Concorde.Handles.Market.Get
                (Concorde.Db.Market.First_Reference_By_Top_Record
                     (Concorde.Db.R_Market))));

      return UI_Type (Result);
   end Get_Nazar_UI;

   -----------
   -- Start --
   -----------

   overriding procedure Start (UI : in out Root_Nazar_UI) is
   begin
      UI.Top.Show;
   end Start;

   ----------
   -- Stop --
   ----------

   overriding procedure Stop
     (UI      : in out Root_Nazar_UI;
      Message : String)
   is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, Message);
      UI.Top.Destroy;
   end Stop;

end Concorde.UI.Nazar_UI;
