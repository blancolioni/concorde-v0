with Concorde.Contexts;

package body Concorde.Commands.System.Change_Scope is

   type Change_Scope_Record is
     new Root_Concorde_Command with null record;

   overriding procedure Perform
     (Command   : Change_Scope_Record;
      Context   : in out Concorde.Contexts.Context_Type;
      Writer    : in out Concorde.Writers.Writer_Interface'Class;
      Arguments : Argument_List);

   --------------------------
   -- Change_Scope_Command --
   --------------------------

   function Change_Scope_Command return Root_Concorde_Command'Class is
   begin
      return Command : Change_Scope_Record;
   end Change_Scope_Command;

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : Change_Scope_Record;
      Context   : in out Concorde.Contexts.Context_Type;
      Writer    : in out Concorde.Writers.Writer_Interface'Class;
      Arguments : Argument_List)
   is
      pragma Unreferenced (Command);
   begin
      if Argument_Count (Arguments) = 0 then
         Context.Set_Default_Scope;
         return;
      end if;

      if Argument_Count (Arguments) /= 1 then
         Writer.Put_Error ("Usage: cd <path>");
         return;
      end if;

      declare
         Scope   : constant String := Argument (Arguments, 1);
         Success : constant Boolean :=
           Context.Change_Scope (Scope);
      begin
         if not Success then
            Writer.Put_Error
              ("Invalid context: " & Scope);
         end if;

      end;

   end Perform;

end Concorde.Commands.System.Change_Scope;
