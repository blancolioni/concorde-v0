package Concorde.UI.Models.Data_Source is

   type Root_Data_Source_Model is
     abstract new Root_Concorde_Model with private;

   function Column_Count
     (Data_Source : Root_Data_Source_Model)
      return Natural
      is abstract;

   function Row_Count
     (Data_Source : Root_Data_Source_Model)
      return Natural
      is abstract;

   function Column_Heading
     (Data_Source : Root_Data_Source_Model;
      Column      : Positive)
      return String
      is abstract
     with Pre'Class => Column <= Data_Source.Column_Count;

   function Cell_Value
     (Data_Source : Root_Data_Source_Model;
      Row         : Positive;
      Column      : Positive)
      return String
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

end Concorde.UI.Models.Data_Source;
