with Concorde.Commands.System;
with Concorde.Commands.Views;

package body Concorde.Commands.Loader is

   -------------------
   -- Load_Commands --
   -------------------

   procedure Load_Commands is
   begin
      Concorde.Commands.System.Load_System_Commands;
      Concorde.Commands.Views.Load_View_Commands;
   end Load_Commands;

end Concorde.Commands.Loader;
