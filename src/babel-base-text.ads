-----------------------------------------------------------------------
--  babel-base -- Database for files
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

with Babel.Files.Sets;
with Babel.Files.Lifecycles;
package Babel.Base.Text is

   type Text_Database is new Database with private;

   --  Insert the file in the database.
   overriding
   procedure Insert (Into : in out Text_Database;
                     File : in Babel.Files.File_Type);

--
--     --  The `On_Create` procedure is called by `Notify_Create` to notify the creation of the item.
--     procedure On_Create (Instance : in Text_Database;
--                          Item     : in Babel.Files.File_Type);
--
--     --  The `On_Update` procedure is called by `Notify_Update` to notify the update of the item.
--     procedure On_Update (Instance : in Text_Database;
--                          Item     : in Babel.Files.File_Type);
--
--     --  The `On_Delete` procedure is called by `Notify_Delete` to notify the deletion of the item.
--     procedure On_Delete (Instance : in Text_Database;
--                          Item     : in Babel.Files.File_Type);
--  Write the SHA1 checksum for the files stored in the map.
--  ------------------------------
   procedure Save (Database : in Text_Database;
                   Path     : in String);

private

   type Text_Database is new Database with record
      Files : Babel.Files.Sets.File_Set;
   end record;

end Babel.Base.Text;
