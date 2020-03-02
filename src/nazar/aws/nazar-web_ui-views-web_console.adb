package body Nazar.Web_UI.Views.Web_Console is

   ----------------
   -- Handle_Get --
   ----------------

   overriding function Handle_Get
     (Handler    : Root_Web_Console_View;
      Session    : Nazar.Sessions.Nazar_Session;
      Parameters : Nazar.Web_UI.Routes.Parameter_Container'Class)
      return Nazar.Json.Json_Value'Class
   is
      pragma Unreferenced (Handler, Session, Parameters);
   begin
      return Nazar.Json.Null_Value;
   end Handle_Get;

   -----------------
   -- Handle_Post --
   -----------------

   overriding function Handle_Post
     (Handler    : Root_Web_Console_View;
      Session    : Nazar.Sessions.Nazar_Session;
      Parameters : Nazar.Web_UI.Routes.Parameter_Container'Class)
      return Nazar.Json.Json_Value'Class
   is
      pragma Unreferenced (Session);
      Line : constant String := Parameters.Parameter ("command");
   begin
      if Line /= "" then
         Handler.Emit_Command_Signal (Command => Line);
      end if;
      return Nazar.Json.String_Value ("");
   end Handle_Post;

   -------------------
   -- Model_Changed --
   -------------------

   overriding procedure Model_Changed (View : in out Root_Web_Console_View) is
   begin
      null;
   end Model_Changed;

   ---------------------
   -- Set_Prompt_Text --
   ---------------------

   overriding procedure Set_Prompt_Text
     (View : in out Root_Web_Console_View; Prompt_Text : String)
   is
   begin
      View.Prompt := Ada.Strings.Unbounded.To_Unbounded_String (Prompt_Text);
   end Set_Prompt_Text;

   ----------
   -- Show --
   ----------

   overriding procedure Show (View : in out Root_Web_Console_View) is
   begin
      null;
   end Show;

   ----------------------
   -- Web_Console_View --
   ----------------------

   function Web_Console_View
     (Model : not null access
        Nazar.Models.Text_Writer.Root_Text_Writer_Model'Class)
      return Nazar_Web_Console_View
   is
   begin
      return View : constant Nazar_Web_Console_View :=
        new Root_Web_Console_View
      do
         View.Set_Model (Model);
      end return;
   end Web_Console_View;

end Nazar.Web_UI.Views.Web_Console;
