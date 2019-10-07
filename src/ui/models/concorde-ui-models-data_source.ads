with Concorde.UI.Models.Renderers;
with Concorde.UI.Models.Values;

package Concorde.UI.Models.Data_Source is

   type Root_Data_Source_Model is
     abstract new Root_Concorde_Model with private;

   overriding function Default_View_Name
     (Model : Root_Data_Source_Model)
      return String;

   function Column_Count
     (Data_Source : Root_Data_Source_Model)
      return Natural
      is abstract;

   function Row_Count
     (Data_Source : Root_Data_Source_Model)
      return Natural
      is abstract;

   function Column_Heading_Id
     (Data_Source : Root_Data_Source_Model;
      Column      : Positive)
      return String
      is abstract
     with Pre'Class => Column <= Data_Source.Column_Count;

   function Column_Heading_Label
     (Data_Source : Root_Data_Source_Model;
      Column      : Positive)
      return String
      is abstract
     with Pre'Class => Column <= Data_Source.Column_Count;

   function Column_Renderer
     (Data_Source : Root_Data_Source_Model;
      Column      : Positive)
      return Renderers.Render_Interface'Class
      is abstract
     with Pre'Class => Column <= Data_Source.Column_Count;

   function Column_Type
     (Data_Source : Root_Data_Source_Model;
      Column      : Positive)
      return Values.Model_Value_Data_Type
      is abstract
     with Pre'Class => Column <= Data_Source.Column_Count;

   function Cell_Value
     (Data_Source : Root_Data_Source_Model;
      Row         : Positive;
      Column      : Positive)
      return Values.Model_Value_Type
      is abstract
     with Pre'Class => Row <= Data_Source.Row_Count
     and then Column <= Data_Source.Column_Count;

   overriding function Handle
     (Model   : in out Root_Data_Source_Model;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Concorde.Json.Json_Value'Class)
      return Concorde.Json.Json_Value'Class;

private

   type Root_Data_Source_Model is
     abstract new Root_Concorde_Model with
      record
         null;
      end record;

   overriding function Default_View_Name
     (Model : Root_Data_Source_Model)
      return String
   is ("Table");

end Concorde.UI.Models.Data_Source;
