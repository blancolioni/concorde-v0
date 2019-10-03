package body Concorde.Commands.System.Cat is

   type Cat_Command_Record is
     new Root_Concorde_Command with null record;

   overriding procedure Perform
     (Command   : Cat_Command_Record;
      Session   : in out Concorde.Sessions.Concorde_Session;
      Context   : in out Concorde.Contexts.Context_Type;
      Writer    : in out Writer_Interface'Class;
      Arguments : Argument_List);

   -----------------
   -- Cat_Command --
   -----------------

   function Cat_Command return Root_Concorde_Command'Class is
   begin
      return Command : Cat_Command_Record;
   end Cat_Command;

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : Cat_Command_Record;
      Session   : in out Concorde.Sessions.Concorde_Session;
      Context   : in out Concorde.Contexts.Context_Type;
      Writer    : in out Writer_Interface'Class;
      Arguments : Argument_List)
   is
      pragma Unreferenced (Command, Session);
   begin
      if Argument_Count (Arguments) = 0 then
         Writer.Put_Error ("Usage: cat file [ files ... ]");
         return;
      end if;

      Context.Push_Scope;

      for I in 1 .. Argument_Count (Arguments) loop
         if not Context.Change_Scope (Argument (Arguments, I)) then
            Writer.Put_Error (Argument (Arguments, I) & ": not found");
            exit;
         end if;

         Writer.Put_Line (Context.Current_Node.Contents);

      end loop;

      Context.Pop_Scope;

   end Perform;

end Concorde.Commands.System.Cat;
