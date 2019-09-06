private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Vectors;

with Gnoga.Types;

private with Gnoga.Gui.View;
with Gnoga.Gui.Window;

private with WL.Guids;
private with WL.String_Maps;

limited with Concorde.UI.Views;

with Concorde.Contexts;
with Concorde.Signals;

with Concorde.Db;

package Concorde.Sessions is

   Signal_Clock_Tick : constant Concorde.Signals.Signal_Type :=
                         "signal-clock-tick";

   type Root_Concorde_Session is
     new Gnoga.Types.Connection_Data_Type
     and Concorde.Signals.Signaler
   with private;

   overriding procedure Send_Signal
     (Session : in out Root_Concorde_Session;
      Signal  : Concorde.Signals.Signal_Type);

   overriding function Add_Handler
     (Session : in out Root_Concorde_Session;
      Signal  : Concorde.Signals.Signal_Type;
      Handler : Concorde.Signals.Handler_Type;
      Data    : Concorde.Signals.Signal_Data_Interface'Class)
     return Concorde.Signals.Handler_Id;

   overriding procedure Remove_Handler
     (Session : in out Root_Concorde_Session;
      Signal  : Concorde.Signals.Signal_Type;
      Id      : Concorde.Signals.Handler_Id);

   procedure Connect
     (Session     : not null access Root_Concorde_Session'Class;
      Main_Window : Gnoga.Gui.Window.Pointer_To_Window_Class);

   procedure Login
     (Session : not null access Root_Concorde_Session'Class;
      User    : Concorde.Db.User_Reference);

   procedure Logout (Session : not null access Root_Concorde_Session'Class);

   function User_Name
     (Session : Root_Concorde_Session'Class)
      return String;

   function Administrator
     (Session : Root_Concorde_Session'Class)
      return Boolean;

   function Faction
     (Session : Root_Concorde_Session'Class)
      return Concorde.Db.Faction_Reference;

   function Main_View
     (Session : Root_Concorde_Session'Class)
      return access Concorde.UI.Views.Root_View_Type'Class;

   function Active_View
     (Session : Root_Concorde_Session'Class)
      return access Concorde.UI.Views.Root_View_Type'Class;

   procedure Activate_View
     (Session : in out Root_Concorde_Session'Class;
      View    : access Concorde.UI.Views.Root_View_Type'Class);

   function Current_Context
     (Session : Root_Concorde_Session'Class)
      return Concorde.Contexts.Context_Path;

   procedure Update_Context
     (Session : in out Root_Concorde_Session'Class;
      Context : Concorde.Contexts.Context_Path);

   function Environment_Value
     (Session : Root_Concorde_Session'Class;
      Name    : String;
      Default : String := "")
      return String;

   procedure Set_Environment_Value
     (Session : in out Root_Concorde_Session'Class;
      Name    : String;
      Value   : String);

   procedure Add_To_History
     (Session      : in out Root_Concorde_Session'Class;
      Command_Line : String);

   function History_Length
     (Session : Root_Concorde_Session'Class)
      return Natural;

   function History
     (Session : Root_Concorde_Session'Class;
      Index   : Integer)
      return String
     with Pre => Index in -Session.History_Length .. Session.History_Length
     and then Index /= 0;

   type Concorde_Session is access all Root_Concorde_Session'Class;

   function New_Gnoga_Session return Concorde_Session;
   function New_Repl_Session
     (User : Concorde.Db.User_Reference)
      return Concorde_Session;

   procedure End_Session (Session : in out Concorde_Session);

   procedure End_All_Sessions;

   procedure Broadcast (Signal : Concorde.Signals.Signal_Type);

private

   type View_Access is access all Concorde.UI.Views.Root_View_Type'Class;

   package View_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (View_Access);

   package Context_Holders is
     new Ada.Containers.Indefinite_Holders
       (Concorde.Contexts.Context_Type, Concorde.Contexts."=");

   package Environment_Maps is
     new WL.String_Maps (String);

   package History_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Root_Concorde_Session is
     new Gnoga.Types.Connection_Data_Type
     and Concorde.Signals.Signaler with
      record
         Id            : WL.Guids.Guid;
         Is_Gnoga      : Boolean := False;
         Main_Window   : Gnoga.Gui.Window.Pointer_To_Window_Class;
         Administrator : Boolean := False;
         User          : Concorde.Db.User_Reference :=
                           Concorde.Db.Null_User_Reference;
         Faction       : Concorde.Db.Faction_Reference :=
                           Concorde.Db.Null_Faction_Reference;
         Context       : Concorde.Contexts.Context_Path;
         Current_View  : View_Access;
         Active_View   : View_Access;
         Views         : View_Lists.List;
         Dispatcher    : Concorde.Signals.Signal_Dispatcher;
         Environment   : Environment_Maps.Map;
         History       : History_Vectors.Vector;
      end record;

   function Administrator
     (Session : Root_Concorde_Session'Class)
      return Boolean
   is (Session.Administrator);

   function Faction
     (Session : Root_Concorde_Session'Class)
      return Concorde.Db.Faction_Reference
   is (Session.Faction);

   function Current_Context
     (Session : Root_Concorde_Session'Class)
      return Concorde.Contexts.Context_Path
   is (Session.Context);

   function Main_View
     (Session : Root_Concorde_Session'Class)
      return access Concorde.UI.Views.Root_View_Type'Class
   is (Session.Current_View);

   function Active_View
     (Session : Root_Concorde_Session'Class)
      return access Concorde.UI.Views.Root_View_Type'Class
   is (Session.Active_View);

   function Find_Session
     (Gnoga_View : Gnoga.Gui.View.Pointer_To_View_Base_Class)
      return Concorde_Session;

   function Environment_Value
     (Session : Root_Concorde_Session'Class;
      Name    : String;
      Default : String := "")
      return String
   is (if Session.Environment.Contains (Name)
       then Session.Environment.Element (Name)
       else Default);

   function History_Length
     (Session : Root_Concorde_Session'Class)
      return Natural
   is (Session.History.Last_Index);

   function History
     (Session : Root_Concorde_Session'Class;
      Index   : Integer)
      return String
   is (if Index > 0
       then Session.History.Element (Index)
       else Session.History.Element (Session.History.Last_Index + Index + 1));

end Concorde.Sessions;
