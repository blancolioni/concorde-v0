package body Concorde.UI.Views is

   ---------
   -- Add --
   ---------

   procedure Add
     (List : in out Draw_Command_List'Class; Command : Draw_Command)
   is
   begin
      List.Commands.Append (Command);
   end Add;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (List    : Draw_Command_List'Class;
      Process : not null access
        procedure (Command : Draw_Command'Class))
   is
   begin
      for Command of List.Commands loop
         Process (Command);
      end loop;
   end Iterate;

end Concorde.UI.Views;
