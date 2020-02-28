with Nazar.Models.Environment;

with Concorde.Version;

with Concorde.UI.Entities;

package body Concorde.UI.Models.Console is

   -------------------
   -- Console_Model --
   -------------------

   function Console_Model return Concorde_Console_Model is
      Environment : constant Nazar.Models.Environment.Nazar_Environment_Model
        := new Nazar.Models.Environment.Root_Environment_Model;
   begin
      Environment.Set_Environment_Value
        ("VERSION", Concorde.Version.Version_String);
      return Model : constant Concorde_Console_Model :=
        new Root_Console_Model
      do
         Model.Initialize
           (Concorde.UI.Entities.Root, Environment, "/");
      end return;
   end Console_Model;

end Concorde.UI.Models.Console;
