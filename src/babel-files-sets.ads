-----------------------------------------------------------------------
--  babel-files-sets -- Sets of files
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

with Ada.Containers.Ordered_Sets;
with Util.Strings;

package Babel.Files.Sets is

   package File_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => File_Type,
                                  "<"          => "<",
                                  "="          => "=");

   subtype File_Set is File_Sets.Set;
   subtype File_Cursor is File_Sets.Cursor;

   --  Insert the file in the file set.
   procedure Insert (Into : in out File_Set;
                     File : in File_Type);

end Babel.Files.Sets;
