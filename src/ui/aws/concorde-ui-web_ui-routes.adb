with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with WL.Guids;

with AWS.Messages;
with AWS.Parameters;
with AWS.Response.Set;

package body Concorde.UI.Web_UI.Routes is

   package Request_Handler_Holders is
     new Ada.Containers.Indefinite_Holders
       (Request_Handler'Class);

   type Route_Record is
      record
         Template : Ada.Strings.Unbounded.Unbounded_String;
         Handler  : Request_Handler_Holders.Holder;
      end record;

   package Route_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Route_Record);

   type Method_Route_Array is
     array (AWS.Status.Request_Method) of Route_Lists.List;

   Routes : Method_Route_Array;

   type State_Access is access all State_Interface'Class;

   package State_Maps is
     new WL.String_Maps (State_Access);

   States : State_Maps.Map;

   function Match
     (Template : String;
      URI      : String)
      return Boolean;

   function Get_Parameters
     (Request    : AWS.Status.Data;
      Template   : String)
     return Parameter_Container;

   ---------------
   -- Add_Route --
   ---------------

   procedure Add_Route
     (Method  : AWS.Status.Request_Method;
      Path    : String;
      Handler : Request_Handler'Class)
   is
   begin
      Routes (Method).Append
        (Route_Record'
           (Ada.Strings.Unbounded.To_Unbounded_String (Path),
            Request_Handler_Holders.To_Holder (Handler)));
   end Add_Route;

   --------------------
   -- Get_Parameters --
   --------------------

   function Get_Parameters
     (Request    : AWS.Status.Data;
      Template   : String)
      return Parameter_Container
   is
      pragma Unreferenced (Template);
      Ps : constant AWS.Parameters.List :=
        AWS.Status.Parameters (Request);
   begin
      return Result : Parameter_Container do
         for I in 1 .. Ps.Count loop
            declare
               Name  : constant String :=
                 Ps.Get_Name (I);
               Value : constant String :=
                 Ps.Get_Value (I);
            begin
               if Result.Contains (Name) then
                  Result.Replace (Name, Value);
               else
                  Result.Insert (Name, Value);
               end if;
            end;
         end loop;
      end return;
   end Get_Parameters;

   ------------
   -- Handle --
   ------------

   function Handle (Request : AWS.Status.Data) return AWS.Response.Data is
      Method : constant AWS.Status.Request_Method :=
        AWS.Status.Method (Request);
   begin
      for Route of Routes (Method) loop
         if Match
           (Template => Ada.Strings.Unbounded.To_String (Route.Template),
            URI      => AWS.Status.URI (Request))
         then
            declare
               Handler : constant Request_Handler'Class :=
                 Route.Handler.Element;
               Parameters : constant Parameter_Container :=
                 Get_Parameters
                   (Request,
                    Ada.Strings.Unbounded.To_String (Route.Template));
               State_Id   : constant String :=
                 Parameters.Parameter ("id");
               Response   : AWS.Response.Data;
            begin
               Ada.Text_IO.Put_Line
                 (Method'Image & " "
                  & AWS.Status.URI (Request)
                  & " [" & State_Id & "]");

               case Method is
                  when AWS.Status.GET =>
                     if State_Id = ""
                       or else not States.Contains (State_Id)
                     then
                        Response := AWS.Response.Build
                          (Content_Type  => "text/plain",
                           Message_Body  => "invalid session id",
                           Status_Code   => AWS.Messages.S404);
                     else
                        declare
                           Result_Json : constant Json.Json_Value'Class :=
                             Handle_Get
                               (Handler    => Handler,
                                State      => States.Element (State_Id).all,
                                Parameters => Parameters);
                        begin
                           Response := AWS.Response.Build
                             (Content_Type  => "text/plain",
                              Message_Body  => Json.Serialize (Result_Json));
                        end;
                     end if;
                  when AWS.Status.POST =>
                     if (not Handler.Creates_State
                         and then State_Id = "")
                       or else
                         (State_Id /= ""
                          and then not States.Contains (State_Id))
                     then
                        Response := AWS.Response.Build
                          (Content_Type  => "text/plain",
                           Message_Body  => "invalid session id",
                           Status_Code   => AWS.Messages.S404);
                     elsif State_Id = "" then
                        declare
                           Id        : constant String :=
                             WL.Guids.To_String
                               (WL.Guids.New_Guid);
                           New_State : constant State_Interface'Class :=
                             Handler.Handle_Create (Parameters);
                           Json      : Concorde.Json.Json_Object;
                           Status    : constant AWS.Messages.Status_Code :=
                             (if New_State.Valid
                              then AWS.Messages.S200
                              else AWS.Messages.S401);
                        begin
                           if New_State.Valid then
                              declare
                                 State     : constant State_Access :=
                                   new State_Interface'Class'(New_State);
                              begin
                                 States.Insert (Id, State);
                                 Json.Set_Property ("id", Id);
                                 Json.Set_Property ("user", State.User_Name);
                              end;
                           else
                              Json.Set_Property ("error", "login failed");
                           end if;
                           Response :=
                             AWS.Response.Build
                               ("text/json", Json.Serialize, Status);
                        end;
                     else
                        declare
                           State : constant State_Access :=
                             States.Element (State_Id);
                           Result_Json : constant Json.Json_Value'Class :=
                             Handle_Post
                               (Handler    => Handler,
                                State      => State.all,
                                Parameters => Parameters);
                        begin
                           Response := AWS.Response.Build
                             (Content_Type  => "text/plain",
                              Message_Body  => Json.Serialize (Result_Json));
                        end;
                     end if;

                  when others =>

                     Response := AWS.Response.Build
                       (Content_Type  => "text/plain",
                        Message_Body  => "Method not allowed",
                        Status_Code   => AWS.Messages.S405);
               end case;

               AWS.Response.Set.Add_Header
                 (Response,
                  "Access-Control-Allow-Origin",
                  "http://localhost:3000");

               return Response;

            end;
         end if;
      end loop;

      return AWS.Response.Build
        (Content_Type  => "text/plain",
         Message_Body  => "Not found",
         Status_Code   => AWS.Messages.S404);
   end Handle;

   -------------------
   -- Handle_Create --
   -------------------

   function Handle_Create
     (Handler    : Request_Handler;
      Parameters : Parameter_Container'Class)
      return State_Interface'Class
   is
      pragma Unreferenced (Handler, Parameters);
   begin
      return (raise Route_Error with "bad GET request");
   end Handle_Create;

   ----------------
   -- Handle_Get --
   ----------------

   function Handle_Get
     (Handler    : Request_Handler;
      State      : State_Interface'Class;
      Parameters : Parameter_Container'Class)
      return Concorde.Json.Json_Value'Class
   is
      pragma Unreferenced (Handler, State, Parameters);
   begin
      return (raise Route_Error with "bad GET request");
   end Handle_Get;

   -----------------
   -- Handle_Post --
   -----------------

   function Handle_Post
     (Handler    : Request_Handler;
      State      : in out State_Interface'Class;
      Parameters : Parameter_Container'Class)
      return Concorde.Json.Json_Value'Class
   is
      pragma Unreferenced (Handler, State, Parameters);
   begin
      return (raise Route_Error with "bad POST request");
   end Handle_Post;

   -----------
   -- Match --
   -----------

   function Match
     (Template : String;
      URI      : String)
      return Boolean
   is
   begin
      return Template = URI;
   end Match;

   ---------------
   -- Parameter --
   ---------------

   function Parameter
     (Container : Parameter_Container;
      Name      : String)
      return String
   is
      Position : constant String_Maps.Cursor :=
        Container.Find (Name);
   begin
      if String_Maps.Has_Element (Position) then
         return String_Maps.Element (Position);
      else
         return "";
      end if;
   end Parameter;

end Concorde.UI.Web_UI.Routes;
