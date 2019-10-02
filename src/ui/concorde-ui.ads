package Concorde.UI is

   type Client_Id is new Natural;

   type UI_Interface is limited interface;

   procedure Start
     (Item  : in out UI_Interface)
   is abstract;

   type State_Interface is interface;

   function Valid
     (State : State_Interface)
      return Boolean
      is abstract;

   function User_Name
     (State : State_Interface)
      return String
   is abstract
     with Pre'Class => State.Valid;

   function Is_Administrator
     (State : State_Interface)
      return Boolean
      is abstract
     with Pre'Class => State.Valid;

   function New_Client
     (State : in out State_Interface)
      return Client_Id
      is abstract;

   procedure Close_Client
     (State  : in out State_Interface;
      Client : Client_Id)
   is abstract;

   type State_Update_Interface is interface;

   procedure Execute
     (Update : State_Update_Interface;
      State  : in out State_Interface'Class)
   is abstract;

end Concorde.UI;
