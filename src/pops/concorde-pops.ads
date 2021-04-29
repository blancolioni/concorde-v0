with Concorde.Handles.Colony;
with Concorde.Handles.Employer;
with Concorde.Handles.Faction;
with Concorde.Handles.Pop;
with Concorde.Handles.Pop_Group;
with Concorde.Handles.World_Sector;

with Concorde.Money;
with Concorde.Quantities;

package Concorde.Pops is

   procedure New_Pop
     (Faction : Concorde.Handles.Faction.Faction_Class;
      Colony  : Concorde.Handles.Colony.Colony_Class;
      Sector  : Concorde.Handles.World_Sector.World_Sector_Class;
      Group   : Concorde.Handles.Pop_Group.Pop_Group_Class;
      Size    : Concorde.Quantities.Quantity_Type;
      Cash    : Concorde.Money.Money_Type);

   procedure Load_Pops;

   function Describe (Pop : Concorde.Handles.Pop.Pop_Class) return String;
   procedure Log (Pop     : Concorde.Handles.Pop.Pop_Class;
                  Message : String);

   procedure On_Employment
     (Pop      : Concorde.Handles.Pop.Pop_Class;
      Employer : Concorde.Handles.Employer.Employer_Class;
      Quantity : Concorde.Quantities.Quantity_Type;
      Salary   : Concorde.Money.Price_Type);

end Concorde.Pops;
