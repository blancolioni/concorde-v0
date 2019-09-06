with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Properties;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Db;

package Concorde.Commodities is

   subtype Commodity_Reference is Concorde.Db.Commodity_Reference;

   function Local_Name
     (Commodity : Commodity_Reference)
      return String;

   function Initial_Price
     (Commodity : Commodity_Reference)
      return Concorde.Money.Price_Type;

   function Has_Property
     (Commodity : Commodity_Reference;
      Property  : Concorde.Properties.Property_Type)
      return Boolean;

   function Get_Property
     (Commodity : Commodity_Reference;
      Property  : Concorde.Properties.Property_Type)
      return Real;

   function Get
     (Tag : String)
      return Commodity_Reference;

   function Exists
     (Tag : String)
      return Boolean;

   package Commodity_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Concorde.Db.Commodity_Reference, Concorde.Db."=");

   function All_Commodities return Commodity_Lists.List;
   function Skills return Commodity_Lists.List;

   function Is_Skill (Commodity : Commodity_Reference) return Boolean;

   function Lease_Commodities
     (For_Commodity : Commodity_Reference)
      return Commodity_Lists.List;

   function Lease
     (Commodity : Commodity_Reference;
      Days      : Positive)
      return Commodity_Reference;

   function Is_Lease
     (Commodity : Commodity_Reference)
      return Boolean;

   function Leased_Commodity
     (Commodity : Commodity_Reference)
      return Commodity_Reference
     with Pre => Is_Lease (Commodity);

   function Lease_Days
     (Commodity : Commodity_Reference)
      return Positive
     with Pre => Is_Lease (Commodity);

   function Lease_Category
     return Concorde.Db.Commodity_Class_Reference;

   function Title_Category
     return Concorde.Db.Commodity_Class_Reference;

   function Title_Tag
     (Sector     : Concorde.Db.World_Sector_Reference;
      Title_Type : Commodity_Reference)
      return String;

   function Title_Commodity
     (Sector     : Concorde.Db.World_Sector_Reference;
      Title_Zone : Commodity_Reference)
      return Commodity_Reference;

   function Is_Title
     (Commodity : Commodity_Reference)
      return Boolean;

   type Stock_Type is tagged private;

   function Get_Quantity
     (Stock     : Stock_Type;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type;

   function Get_Value
     (Stock     : Stock_Type;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Money.Money_Type;

   function Get_Price_Per_Item
     (Stock     : Stock_Type;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Money.Price_Type;

   procedure Set_Quantity
     (Stock     : in out Stock_Type;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Value     : Concorde.Money.Money_Type);

   procedure Set_Quantity
     (Stock     : in out Stock_Type;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price_Per : Concorde.Money.Price_Type);

   function Total_Quantity
     (Stock     : Stock_Type)
      return Concorde.Quantities.Quantity_Type;

   function Total_Value
     (Stock     : Stock_Type)
      return Concorde.Money.Money_Type;

   procedure Iterate
     (Stock : Stock_Type;
      Process : not null access
        procedure (Commodity : Concorde.Db.Commodity_Reference;
                   Quantity  : Concorde.Quantities.Quantity_Type;
                   Value     : Concorde.Money.Money_Type));

   function Missing
     (Available_Stock : Stock_Type;
      Required_Stock  : Stock_Type)
      return Stock_Type;

   procedure Add
     (To    : in out Stock_Type;
      Other : Stock_Type);

   procedure Load
     (Stock     : in out Stock_Type;
      Has_Stock : Concorde.Db.Has_Stock_Reference);

private

   type Stock_Record is
      record
         Commodity : Commodity_Reference;
         Quantity  : Concorde.Quantities.Quantity_Type;
         Value     : Concorde.Money.Money_Type;
      end record;

   package Stock_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Stock_Record);

   type Stock_Type is tagged
      record
         List           : Stock_Lists.List;
         Total_Quantity : Concorde.Quantities.Quantity_Type :=
                            Concorde.Quantities.Zero;
         Total_Value    : Concorde.Money.Money_Type :=
                            Concorde.Money.Zero;
      end record;

end Concorde.Commodities;
