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
with Ada.Strings.Unbounded;
with Ada.Directories;
with Util.Strings.Sets;
package Babel.Filters is

   type Filter_Type is limited interface;
   type Filter_Type_Access is access all Filter_Type'Class;

   --  Return True if the file or directory is accepted by the filter.
   function Is_Accepted (Filter : in Filter_Type;
                         Kind   : in Ada.Directories.File_Kind;
                         Path   : in String;
                         Name   : in String) return Boolean is abstract;

   --  Filter to exclude directories
   type Exclude_Directory_Filter_Type is new Filter_Type with private;

   --  Return True if the file or directory is accepted by the filter.
   overriding
   function Is_Accepted (Filter : in Exclude_Directory_Filter_Type;
                         Kind   : in Ada.Directories.File_Kind;
                         Path   : in String;
                         Name   : in String) return Boolean;

   --  Add the given path to the list of excluded directories.
   procedure Add_Exclude (Filter : in out Exclude_Directory_Filter_Type;
                          Path   : in String);

private

   type Exclude_Directory_Filter_Type is new Filter_Type with record
      Excluded : Util.Strings.Sets.Set;
   end record;

end Babel.Filters;
