with Tropos.Reader;

with Concorde.Identifiers;

with Concorde.Handles.Sector_Use;
with Concorde.Handles.Zone;

with Concorde.Db;

package body Concorde.Configure.Zones is

   ---------------------
   -- Configure_Zones --
   ---------------------

   procedure Configure_Zones
     (Scenario_Name : String)
   is

      Finished : Boolean := False;

      procedure Configure
        (Config : Tropos.Configuration);

      ---------------
      -- Configure --
      ---------------

      procedure Configure
        (Config : Tropos.Configuration)
      is
         Tag : constant String := Config.Config_Name;
         Parent_Name : constant String := Config.Get ("parent", "");
         Parent_Use  : constant Handles.Sector_Use.Sector_Use_Handle :=
           Handles.Sector_Use.Get_By_Tag (Parent_Name);
      begin
         if Handles.Zone.Get_By_Tag (Tag).Has_Element then
            null;
         elsif Parent_Name = ""
           or else Parent_Use.Has_Element
         then
            declare
               Sector_Use : constant Handles.Sector_Use.Sector_Use_Handle :=
                 Concorde.Handles.Sector_Use.Create
                   (Tag             => Tag,
                    Parent          => Parent_Use);
            begin
               for Zone_Config of Config.Child ("zones") loop
                  Concorde.Handles.Zone.Create
                    (Tag             => Zone_Config.Config_Name,
                     Identifier      => Concorde.Identifiers.Next_Identifier,
                     Content         => Concorde.Db.Quantity,
                     Sector_Use      => Sector_Use);
               end loop;
            end;
         else
            Finished := False;
         end if;
      end Configure;

   begin
      while not Finished loop
         Finished := True;

         Tropos.Reader.Read_Config
           (Path      => Scenario_Directory (Scenario_Name, "zones"),
            Extension => "zone",
            Configure => Configure'Access);
      end loop;

   end Configure_Zones;

end Concorde.Configure.Zones;
