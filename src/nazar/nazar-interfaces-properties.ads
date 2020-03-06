private with WL.String_Maps;

with Nazar.Values;

package Nazar.Interfaces.Properties is

   type Property_Container_Interface is interface;

   procedure Declare_Property
     (Container     : in out Property_Container_Interface;
      Property_Name : String;
      Property_Type : Nazar.Values.Nazar_Value_Type;
      Initial_Value : Nazar.Values.Nazar_Value)
   is abstract;

   procedure Declare_Property
     (Container     : in out Property_Container_Interface'Class;
      Property_Name : String;
      Property_Type : Nazar.Values.Nazar_Value_Type);

   procedure Declare_Property
     (Container     : in out Property_Container_Interface'Class;
      Property_Name : String;
      Initial_Value : Nazar.Values.Nazar_Value);

   procedure Declare_Property
     (Container     : in out Property_Container_Interface'Class;
      Property_Name : String;
      Initial_Value : String);

   procedure Declare_Property
     (Container     : in out Property_Container_Interface'Class;
      Property_Name : String;
      Initial_Value : Integer);

   procedure Declare_Property
     (Container     : in out Property_Container_Interface'Class;
      Property_Name : String;
      Initial_Value : Boolean);

   procedure Declare_Properties
     (Container : in out Property_Container_Interface)
   is null;

   function Has_Property
     (Container     : Property_Container_Interface;
      Property_Name : String)
      return Boolean
      is abstract;

   function Get_Property
     (Container     : Property_Container_Interface;
      Property_Name : String)
      return Nazar.Values.Nazar_Value
      is abstract
     with Pre'Class => Container.Has_Property (Property_Name);

   function Get_Property
     (Container     : Property_Container_Interface'Class;
      Property_Name : String;
      Default_Value : Integer)
      return Integer;

   function Get_Property
     (Container     : Property_Container_Interface'Class;
      Property_Name : String;
      Default_Value : String)
      return String;

   procedure Set_Property
     (Container      : in out Property_Container_Interface;
      Property_Name  : String;
      Property_Value : Nazar.Values.Nazar_Value)
   is abstract
     with Pre'Class => Container.Has_Property (Property_Name);

   procedure Set_Property
     (Container      : in out Property_Container_Interface'Class;
      Property_Name  : String;
      Property_Value : String);

   type Root_Property_Container is
     new Property_Container_Interface with private;

private

   type Property_Element_Type is
      record
         Property_Type  : Nazar.Values.Nazar_Value_Type;
         Property_Value : Nazar.Values.Nazar_Value;
      end record;

   package Property_Maps is
     new WL.String_Maps (Property_Element_Type);

   type Root_Property_Container is
     new Property_Container_Interface with
      record
         Map  : Property_Maps.Map;
      end record;

   overriding procedure Declare_Property
     (Container     : in out Root_Property_Container;
      Property_Name : String;
      Property_Type : Nazar.Values.Nazar_Value_Type;
      Initial_Value : Nazar.Values.Nazar_Value);

   overriding function Has_Property
     (Container     : Root_Property_Container;
      Property_Name : String)
      return Boolean;

   overriding function Get_Property
     (Container     : Root_Property_Container;
      Property_Name : String)
      return Nazar.Values.Nazar_Value;

   overriding procedure Set_Property
     (Container      : in out Root_Property_Container;
      Property_Name  : String;
      Property_Value : Nazar.Values.Nazar_Value);

end Nazar.Interfaces.Properties;
