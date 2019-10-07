private with WL.String_Maps;

with AWS.Response;
with AWS.Status;

with Concorde.Json;

package Concorde.UI.Web_UI.Routes is

   Route_Error : exception;

   type Parameter_Container is tagged private;

   function Parameter
     (Container : Parameter_Container;
      Name      : String)
      return String;

   function To_Json
     (Container : Parameter_Container)
      return Concorde.Json.Json_Value'Class;

   type Request_Handler is abstract tagged private;

   function Handle_Create
     (Handler    : Request_Handler;
      Parameters : Parameter_Container'Class)
      return State_Interface'Class;

   function Handle_Get
     (Handler    : Request_Handler;
      State      : State_Interface'Class;
      Parameters : Parameter_Container'Class)
      return Concorde.Json.Json_Value'Class;

   function Handle_Post
     (Handler    : Request_Handler;
      State      : in out State_Interface'Class;
      Parameters : Parameter_Container'Class)
      return Concorde.Json.Json_Value'Class;

   function Creates_State
     (Handler : Request_Handler)
      return Boolean
   is (False);

   procedure Add_Route
     (Method  : AWS.Status.Request_Method;
      Path    : String;
      Handler : Request_Handler'Class);

   function Handle
     (Request : AWS.Status.Data)
      return AWS.Response.Data;

   function Handle_Socket_Message
     (Message : String)
      return String;

private

   package String_Maps is
     new WL.String_Maps (String);

   type Parameter_Container is
     new String_Maps.Map with null record;

   type Request_Handler is abstract tagged
      record
         null;
      end record;

end Concorde.UI.Web_UI.Routes;
