      *
      * CALLJAVA
      *
      * Copyright (C) 2010-2017 Heirloom Computing Inc.  All Rights Reserved.
      *
      * This file and associated files are copyrighted information
      * of Heirloom Computing.  Permission is granted for usage in 
      * conjunction with the Elastic COBOL product.
      *
       identification division.
       program-id. calljava.
      
       data division.
       working-storage section.
      
       77 param-0 pic x(10) value "hello".
       77 param-1 pic 999v99 value 123.45.
       77 param-2 pic 999.99 value 678.90.
       01 param-3.
           05 item-1 pic x(10) value "in group".
           05 item-2 comp-1 value 3.141.
           05 item-3 pic 999 value 987.
       01 param-4 occurs 10 times.
           05 item-element pic x(10).
       01 param-5 object reference "java.lang.String".
       01 param-6 signed-int value -765.
      
       procedure division.
       main-paragraph.
           move "a" to item-element(1)
           move "b" to item-element(2)
           move "c" to item-element(3)
           move "d" to item-element(4)
           move "e" to item-element(5)
           move "f" to item-element(6)
           move "g" to item-element(7)
           move "h" to item-element(8)
           move "i" to item-element(9)
           move "j" to item-element(10)
           
      * Construct an object to pass as an example
           invoke param-5 using by value "Java Object" giving param-5
      
           call "ShowParams" using 
               param-0 param-1 param-2 param-3 param-4 param-5 param-6
      
           display "Returned from Java. Program terminated."
               upon sysout
           .
