package body Nazar.Interfaces.Properties is

   ----------------------
   -- Declare_Property --
   ----------------------

   procedure Declare_Property
     (Container     : in out Property_Container_Interface'Class;
      Property_Name : String;
      Property_Type : Nazar.Values.Nazar_Value_Type)
   is
   begin
      Container.Declare_Property
        (Property_Name => Property_Name,
         Property_Type => Property_Type,
         Initial_Value => Nazar.Values.Default_Value (Property_Type));
   end Declare_Property;

   ----------------------
   -- Declare_Property --
   ----------------------

   procedure Declare_Property
     (Container     : in out Property_Container_Interface'Class;
      Property_Name : String;
      Initial_Value : Nazar.Values.Nazar_Value)
   is
   begin
      Container.Declare_Property
        (Property_Name => Property_Name,
         Property_Type => Nazar.Values.Get_Type (Initial_Value),
         Initial_Value => Initial_Value);
   end Declare_Property;

   ----------------------
   -- Declare_Property --
   ----------------------

   procedure Declare_Property
     (Container     : in out Property_Container_Interface'Class;
      Property_Name : String;
      Initial_Value : String)
   is
   begin
      Container.Declare_Property
        (Property_Name, Nazar.Values.To_Value (Initial_Value));
   end Declare_Property;

   ----------------------
   -- Declare_Property --
   ----------------------

   procedure Declare_Property
     (Container     : in out Property_Container_Interface'Class;
      Property_Name : String;
      Initial_Value : Integer)
   is
   begin
      Container.Declare_Property
        (Property_Name, Nazar.Values.To_Value (Initial_Value));
   end Declare_Property;

   ----------------------
   -- Declare_Property --
   ----------------------

   overriding procedure Declare_Property
     (Container     : in out Root_Property_Container;
      Property_Name : String;
      Property_Type : Nazar.Values.Nazar_Value_Type;
      Initial_Value : Nazar.Values.Nazar_Value)
   is
   begin
      Container.Map.Insert
        (Property_Name,
         Property_Element_Type'
           (Property_Type  => Property_Type,
            Property_Value => Initial_Value));
   end Declare_Property;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Container     : Property_Container_Interface'Class;
      Property_Name : String;
      Default_Value : Integer)
      return Integer
   is
   begin
      if Container.Has_Property (Property_Name) then
         return Nazar.Values.To_Integer
           (Container.Get_Property (Property_Name));
      else
         return Default_Value;
      end if;
   end Get_Property;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Container     : Property_Container_Interface'Class;
      Property_Name : String;
      Default_Value : String)
      return String
   is
   begin
      if Container.Has_Property (Property_Name) then
         return Nazar.Values.To_String
           (Container.Get_Property (Property_Name));
      else
         return Default_Value;
      end if;
   end Get_Property;

   ------------------
   -- Get_Property --
   ------------------

   overriding function Get_Property
     (Container     : Root_Property_Container;
      Property_Name : String)
      return Nazar.Values.Nazar_Value
   is
   begin
      return Container.Map (Property_Name).Property_Value;
   end Get_Property;

   ------------------
   -- Has_Property --
   ------------------

   overriding function Has_Property
     (Container     : Root_Property_Container;
      Property_Name : String)
      return Boolean
   is
   begin
      return Container.Map.Contains (Property_Name);
   end Has_Property;

   ------------------
   -- Set_Property --
   ------------------

   overriding procedure Set_Property
     (Container      : in out Root_Property_Container;
      Property_Name  : String;
      Property_Value : Nazar.Values.Nazar_Value)
   is
   begin
      Container.Map (Property_Name).Property_Value :=
        Property_Value;
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Container      : in out Property_Container_Interface'Class;
      Property_Name  : String;
      Property_Value : String)
   is
   begin
      Container.Set_Property
        (Property_Name, Nazar.Values.To_Value (Property_Value));
   end Set_Property;

end Nazar.Interfaces.Properties;
