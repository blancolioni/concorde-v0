with Concorde.Sessions;

package Concorde.Repl is

   procedure Read
     (Session : Concorde.Sessions.Concorde_Session;
      Path    : String);

   procedure Execute
     (Session : Concorde.Sessions.Concorde_Session);

end Concorde.Repl;
