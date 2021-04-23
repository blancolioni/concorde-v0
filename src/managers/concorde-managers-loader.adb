--  with Concorde.Managers.Armies;
--  with Concorde.Managers.Colonies;
--  with Concorde.Managers.Factions;

--  with Concorde.Managers.Installations;

package body Concorde.Managers.Loader is

   -----------------------
   -- Register_Managers --
   -----------------------

   procedure Register_Managers is
   begin
--        Register.Insert
--          ("default-installation",
--           Concorde.Managers.Installations.Create_Default_Manager'Access);
--        Register.Insert
--          ("default-agora",
--       Concorde.Managers.Installations.Create_Default_Agora_Manager'Access);
--        Register.Insert
--          ("default-pop",
--           Concorde.Managers.Pops.Create_Default_Manager'Access);
--        Register.Insert
--          ("default-faction",
--           Concorde.Managers.Factions.Create_Default_Manager'Access);
--        Register.Insert
--          ("default-army",
--           Concorde.Managers.Armies.Create_Default_Manager'Access);
--        Register.Insert
--          ("faction-company",
--           Concorde.Managers.Factions.Create_Faction_Company_Manager'Access);
      --  Register.Insert
      --    ("default-colony",
--     Concorde.Managers.Colonies.Create_Default_Manager'Access);
      null;
   end Register_Managers;

end Concorde.Managers.Loader;
