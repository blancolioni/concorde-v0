with Ada.Calendar.Formatting;

with Nazar.Models.Environment;

with Concorde.Options;
with Concorde.Version;

with Concorde.Server;

with Concorde.UI.Entities;

package body Concorde.UI.Models.Console is

   -------------------
   -- Console_Model --
   -------------------

   function Console_Model return Concorde_Console_Model is
      Environment : constant Nazar.Models.Environment.Nazar_Environment_Model
        := new Nazar.Models.Environment.Root_Environment_Model;

      procedure Env (Name, Value : String);

      ---------
      -- Env --
      ---------

      procedure Env (Name, Value : String) is
      begin
         Environment.Set_Environment_Value (Name, Value);
      end Env;

   begin
      Env ("LANG", Concorde.Options.Language);
      Env ("START_TIME",
           Ada.Calendar.Formatting.Image
             (Concorde.Server.Start_Time));
      Env ("VERSION", Concorde.Version.Version_String);

      return Model : constant Concorde_Console_Model :=
        new Root_Console_Model
      do
         Model.Initialize
           (Concorde.UI.Entities.Root, Environment, "/");
      end return;
   end Console_Model;

end Concorde.UI.Models.Console;
