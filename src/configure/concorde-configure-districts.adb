with Tropos.Reader;

with Concorde.Handles.Sector_Use;
with Concorde.Handles.District;

package body Concorde.Configure.Districts is

   ---------------------
   -- Configure_Districts --
   ---------------------

   procedure Configure_Districts
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
         if Handles.District.Get_By_Tag (Tag).Has_Element then
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
               for District_Config of Config.Child ("districts") loop
                  Concorde.Handles.District.Create
                    (Tag             => District_Config.Config_Name,
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
           (Path      => Scenario_Directory (Scenario_Name, "districts"),
            Extension => "district",
            Configure => Configure'Access);
      end loop;

   end Configure_Districts;

end Concorde.Configure.Districts;
