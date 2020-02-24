with Concorde.Json;

package Concorde.UI is

   type Client_Id is new Natural;

   type Connection_Interface is interface;

   procedure Send_Message
     (Connection : in out Connection_Interface;
      Message    : Concorde.Json.Json_Value'Class)
   is abstract;

   type UI_Interface is interface;

   procedure Start
     (Item  : in out UI_Interface)
   is abstract;

   procedure Stop
     (Item    : in out UI_Interface;
      Message : String)
   is abstract;

   type Concorde_UI is access all UI_Interface'Class;

   function Current_UI return Concorde_UI;

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
     (State          : in out State_Interface;
      Model_Name     : String;
      Model_Argument : String)
      return Client_Id
      is abstract;

   procedure Replace_Model
     (State          : in out State_Interface;
      Client         : Client_Id;
      Model_Name     : String;
      Model_Argument : String)
      is abstract;

   procedure Close_Client
     (State  : in out State_Interface;
      Client : Client_Id)
   is abstract;

   function Handle_Message
     (State : in out State_Interface;
      Message : Concorde.Json.Json_Value'Class)
      return Concorde.Json.Json_Value'Class
   is abstract;

   function Execute_Command
     (State   : in out State_Interface;
      Client  : Client_Id;
      Command : String)
      return Concorde.Json.Json_Value'Class
   is abstract;

   function Handle_Client_Request
     (State   : in out State_Interface;
      Client  : Client_Id;
      Request : Concorde.Json.Json_Value'Class)
      return Concorde.Json.Json_Value'Class
   is abstract;

   function Environment_Value
     (State : State_Interface;
      Name  : String)
      return Concorde.Json.Json_Value'Class
   is abstract;

   function Environment_Value
     (State : State_Interface'Class;
      Name  : String)
      return String
   is (State.Environment_Value (Name).Image);

private

   Local_Current_UI : Concorde_UI;

   function Current_UI return Concorde_UI is (Local_Current_UI);

end Concorde.UI;
