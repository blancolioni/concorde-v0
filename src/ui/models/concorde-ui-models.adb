with Concorde.Updates.Events;

package body Concorde.UI.Models is

   type Model_Access is access all Regular_Update_Interface'Class;

   type Model_Update is
     new Concorde.Updates.Update_Interface with
      record
         Model : Model_Access;
      end record;

   overriding procedure Activate
     (Update : Model_Update);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate (Update : Model_Update) is
      Continue : Boolean;
   begin
      Update.Model.Refresh (Continue);
      if Continue then
         Concorde.Updates.Events.Update_With_Delay
           (Update.Model.Interval, Update);
      end if;
   end Activate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Model        : not null access Dynamic_Text_Model'Class;
      Initial_Text : String;
      Interval     : Duration)
   is
   begin
      Model.Set_Text (Initial_Text);
      Model.Interval := Interval;
      Model.Start_Updates;
   end Initialize;

   -------------
   -- Refresh --
   -------------

   overriding procedure Refresh
     (Model    : in out Dynamic_Text_Model;
      Continue : out Boolean)
   is
   begin
      Model.Set_Text (Dynamic_Text_Model'Class (Model).Current_Text);
      Continue := True;
   end Refresh;

   -------------------
   -- Start_Updates --
   -------------------

   procedure Start_Updates
     (Model : not null access Regular_Update_Interface'Class)
   is
   begin
      Concorde.Updates.Events.Update_With_Delay
        (Model.Interval, Model_Update'(Model => Model_Access (Model)));
   end Start_Updates;

end Concorde.UI.Models;
