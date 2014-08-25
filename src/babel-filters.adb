-----------------------------------------------------------------------
--  babel-filters -- File filters
--  Copyright (C) 2014 Stephane.Carrez
--  Written by Stephane.Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------

package body Babel.Filters is

   use type Ada.Directories.File_Kind;

   --  ------------------------------
   --  Return True if the file or directory is accepted by the filter.
   --  ------------------------------
   overriding
   function Is_Accepted (Filter : in Exclude_Directory_Filter_Type;
                         Kind   : in Ada.Directories.File_Kind;
                         Path   : in String;
                         Name   : in String) return Boolean is
   begin
      if Kind /= Ada.Directories.Directory then
         return True;
      end if;
      if Filter.Excluded.Contains (Name) then
         return False;
      end if;
      return True;
   end Is_Accepted;

   --  ------------------------------
   --  Add the given path to the list of excluded directories.
   --  ------------------------------
   procedure Add_Exclude (Filter : in out Exclude_Directory_Filter_Type;
                          Path   : in String) is
   begin
      Filter.Excluded.Include (Path);
   end Add_Exclude;

end Babel.Filters;
