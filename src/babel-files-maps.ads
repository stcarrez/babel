-----------------------------------------------------------------------
--  bkp-files -- File and directories
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

with Ada.Containers.Hashed_Maps;

package Babel.Files.Maps is

   --  Babel.Base.Get_File_Map (Directory, File_Map);
   --  Babel.Base.Get_Directory_Map (Directory, Dir_Map);
   --  File_Map.Find (New_File);
   --  Dir_Map.Find (New_File);
   --  Hash string -> File
   package File_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type        => Util.Strings.Name_Access,
                                 Element_Type    => File_Type,
                                 Hash            => Util.Strings.Hash,
                                 Equivalent_Keys => Util.Strings."=",
                                 "="             => "=");

   --  Hash string -> Directory
   package Directory_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type        => Util.Strings.Name_Access,
                                 Element_Type    => Directory_Type,
                                 Hash            => Util.Strings.Hash,
                                 Equivalent_Keys => Util.Strings."=",
                                 "="             => "=");

end Babel.Files.Maps;
