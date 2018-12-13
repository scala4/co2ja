      *begin {iscobol}compiler-directives
      *end {iscobol}compiler-directives
      *begin {iscobol}prog-comment
      *testscreen.cbl
      *testscreen.cbl is generated from D:\Cars2018\testCars\screen\testscreen.isp
      *end {iscobol}prog-comment
       identification division.
      *begin {iscobol}progid
       program-id. testscreen.
       author. Romilda Jorge.
       date-written. Terça-feira 25 Setembro 2018 13:14:53.
       remarks.
      *end {iscobol}progid
       environment division.
       configuration section.
       special-names.
      *begin {iscobol}activex-def
      *end {iscobol}activex-def
      *begin {iscobol}alphabet
      *end {iscobol}alphabet
      *begin {iscobol}decimal-point
       decimal-point is comma.
      *end {iscobol}decimal-point
       repository.
      *begin {iscobol}repository
      *end {iscobol}repository
       input-output section.
       file-control.
      *begin {iscobol}file-control
      *end {iscobol}file-control
       data division.
       file section.
      *begin {iscobol}file-section
      *end {iscobol}file-section
       working-storage section.
      *begin {iscobol}is-def
       copy "isgui.def".
       copy "iscobol.def".
       copy "iscrt.def".
       copy "isfonts.def".
       copy "isresize.def".
       copy "ismsg.def".
      *end {iscobol}is-def
      *begin {iscobol}copy-working
       77 key-status is special-names crt status pic 9(4) value 0.
          88 exit-pushed value 27.
          88 message-received value 95.
          88 event-occurred value 96.
          88 screen-no-input-field value 97.
          88 screen-time-out value 99.
       77 quit-mode-flag pic s9(5) comp-4 value 0.
       77 window-handle handle of window.
       77 main-window handle of window.
       77 bakgrunn-jpg pic s9(9) comp-4.
       78 78-link value x"f0c1".
       77 FontAwesome-11v0 handle of font.
       77 FontAwesome-8v0 handle of font.
       77 FontAwesome-24v0 handle of font.
       77 btn-title pic n any length.
       77 Calibri-11v0 handle of font.
       77 Calibri-12v0-b handle of font.
       77 Calibri-0v0-b handle of font.
       77 Calibri-8v0-b handle of font.
       01 nbtn-title pic n.
       01 but-title redefines nbtn-title pic x(2).
       77 button-png pic s9(9) comp-4.
       77 button-jpg pic s9(9) comp-4.
       77 bakgrunn-jpg0 pic s9(9) comp-4.
       77 appl-title pic x(10) value "B i l".
       77 screen-1-tp-1-vis pic 9.
       77 screen-1-tp-2-vis pic 9.
       77 hmenu pic s9(9) comp-4.
       77 screen-1-tb-1-hdl handle of window.
       77 FontAwesome-24v00 handle of font.
       77 btn1-title pic n any length.
       78 78-calc value x"f1ec".
       77 kalkulator-png pic s9(9) comp-4.
       77 abc-jpg pic s9(9) comp-4.
       77 FontAwesome-24v01 handle of font.
       77 screen-1-st-1-hdl handle of status-bar.
       77 Calibri-10v0 handle of font.
       77 Calibri-14v0 handle of font.
       77 is-scrMain-bkg-img-lines pic 999V999 comp-4 value 96,9.
       77 is-scrMain-bkg-img-size pic 999V999 comp-4 value 150,7.
       77 is-scrMain-bkg-img-tmp pic s9(9) comp-4.
      *end {iscobol}copy-working
      *begin {iscobol}external-definitions
      *end {iscobol}external-definitions
       linkage section.
      *begin {iscobol}copy-linkage
      *end {iscobol}copy-linkage
       screen section.
      *begin {iscobol}copy-screen
       01 screen-1
       .
          03 screen-1-pb-1 Push-Button
             line 27,0
             column 39,4
             size 6,4 cells 
             lines 6,8 cells 
             font FontAwesome-24v0
             id 1
             layout-data 119
             hint "eretergegbe"
             event procedure screen-1-pb-1-evt-proc
             flat
             title btn-title
             .
          03 screen-1-pb-2 Push-Button
             line 32,4
             column 23,2
             size 16,3 cells 
             lines 7,2 cells 
             foreground-color rgb x#ff0000
             font Calibri-12v0-b
             id 2
             flat
             title "Calibri 12"
             left
             .
          03 screen-1-pb-3 Push-Button
             exception-value 1006
             line 37,4
             column 47,0
             size 127
             lines 39
             font Calibri-8v0-b
             id 3
             layout-data 39
             flat
             title "kjnkjbkj"
             bitmap-handle button-png
             framed
             bitmap-width 1000
             title-position 5
             .
          03 screen-1-pb-4 Push-Button
             line 32,4
             column 6,9
             size 126
             lines 54
             id 4
             layout-data 119
             title "teste"
             bitmap-handle button-jpg
             square
             bitmap-width 2000
             title-position 5
             left
             .
          03 screen-1-tc-1 Tab-Control
             line 13,1
             column 33,2
             size 28,2 cells 
             lines 15,0 cells 
             visible 1
             id 5
             layout-data 119
             no-tab
             vertical
             fixed-width
             multiline
             buttons
             flat-buttons
             allow-container
             bitmap-width 16
             .
          03 screen-1-tp-1 tab-group screen-1-tc-1 tab-group-value 1.
             05 screen-1-cb-1 Check-Box
                line 18,2
                column 40,8
                size 10,5 cells 
                lines 1,7 cells 
                id 10
                title "Check-Box"
                .
          03 screen-1-tp-2 tab-group screen-1-tc-1 tab-group-value 2.
             05 screen-1-bi-1 Bitmap
                line 16,4
                column 45,7
                size 105 pixels 
                lines 70 pixels 
                id 14
                bitmap-scale 2
                bitmap-number 1
                .
          03 screen-1-fr-1 Frame
             line 38,5
             column 25,0
             size 16,5 cells 
             lines 3,6 cells 
             id 8
             .
          03 screen-1-pb-7 Push-Button
             line 3,1
             column 29,3
             size 4,3 cells 
             lines 3,8 cells 
             foreground-color rgb x#08088f
             font FontAwesome-24v0
             id 9
             hint "Avbryt Ordre"
             flat
             title "Avbryt Ordre"
             .
          03 screen-1-lb-1 List-Box
             line 14,4
             column 6,2
             size 19,5 cells 
             lines 13,7 cells 
             background-color rgb x#edf2fc
             font Calibri-14v0
             id 11
             value "5464654"
             border-color rgb x#00ff00
             .
          03 screen-1-gr-1 Grid
             line 3,1
             column 42,6
             size 17,2 cells 
             lines 9,2 cells 
             id 12
             no-box
             column-headings
             row-dividers 1
             heading-background-color rgb x#ebeaea 
             heading-foreground-color rgb x#000000
             cursor-frame-width 3
             num-rows 5
             .
          03 screen-1-br-1 Bar
             line 29,5
             column 8,8
             size 30,7 cells 
             id 13
             .
       01 screen-1-tb-1.
          03 screen-1-pb-5 Push-Button
             line 1,7
             column 1,9
             size 3,5 cells 
             lines 4,1 cells 
             foreground-color rgb x#808000
             font FontAwesome-24v00
             id 6
             flat
             title btn1-title
             .
          03 screen-1-pb-6 Push-Button
             line 1,7
             column 8,7
             size 37
             lines 33
             id 7
             flat
             title "Push-Button"
             bitmap-handle abc-jpg
             square
             .
       01 scrMain
       .
          03 is-scrMain-bkg-img bitmap height-in-cells width-in-cells 
          line 1 col 1 size is-scrMain-bkg-img-size lines 
          is-scrMain-bkg-img-lines bitmap-handle bakgrunn-jpg0.
      *end {iscobol}copy-screen
      *begin {iscobol}procedure-using
       procedure division.
      *end {iscobol}procedure-using
      *begin {iscobol}declarative
      *end {iscobol}declarative
       main-logic.
      *begin {iscobol}entry-bef-prog
      *end {iscobol}entry-bef-prog
      *begin {iscobol}initial-routines
           perform is-initial-routine
      *end {iscobol}initial-routines
      *begin {iscobol}run-main-screen
           perform is-screen-1-routine
      *end {iscobol}run-main-screen
      *begin {iscobol}exit-routines
           perform is-exit-rtn.
      *end {iscobol}exit-routines
      *begin {iscobol}copy-procedure
       copy "ismsg.cpy".
       is-initial-routine.
           accept system-information from system-info.
           accept terminal-abilities from terminal-info.
           perform is-load-fonts.
           perform is-load-bitmaps.
       is-load-fonts.
           initialize wfont-data calibri-14v0.
           move 14 to wfont-size.
           move "Calibri" to wfont-name.
           set wfont-bold to false.
           set wfont-italic to false.
           set wfont-underline to false.
           set wfont-strikeout to false.
           set wfont-fixed-pitch to false.
           call "w$font" using wfont-get-font calibri-14v0 wfont-data.
           initialize wfont-data calibri-8v0-b.
           move 8 to wfont-size.
           move "Calibri" to wfont-name.
           set wfont-bold to true.
           set wfont-italic to false.
           set wfont-underline to false.
           set wfont-strikeout to false.
           set wfont-fixed-pitch to false.
           call "w$font" using wfont-get-font calibri-8v0-b wfont-data.
           initialize wfont-data calibri-12v0-b.
           move 12 to wfont-size.
           move "Calibri" to wfont-name.
           set wfont-bold to true.
           set wfont-italic to false.
           set wfont-underline to false.
           set wfont-strikeout to false.
           set wfont-fixed-pitch to false.
           call "w$font" using wfont-get-font calibri-12v0-b wfont-data.
           initialize wfont-data fontawesome-24v0.
           move 24 to wfont-size.
           move "FontAwesome" to wfont-name.
           set wfont-bold to false.
           set wfont-italic to false.
           set wfont-underline to false.
           set wfont-strikeout to false.
           set wfont-fixed-pitch to false.
           call "w$font" using wfont-get-font fontawesome-24v0 
           wfont-data.
           move fontawesome-24v0 to fontawesome-24v00.
       is-load-bitmaps.
           call "w$bitmap" using wbitmap-load "D:\usr\im5\abc.jpg" 
           giving abc-jpg.
           call "w$bitmap" using wbitmap-load "bakgrunn.jpg" giving 
           bakgrunn-jpg0.
           call "w$bitmap" using wbitmap-load "button.jpg" giving 
           button-jpg.
           call "w$bitmap" using wbitmap-load "button.png" giving 
           button-png.
       is-exit-rtn.
           perform is-destroy-fonts.
           perform is-destroy-bitmaps.
           perform is-destroy-menus.
           exit program.
           stop run.
       is-destroy-fonts.
           destroy calibri-14v0.
           destroy calibri-8v0-b.
           destroy calibri-12v0-b.
           destroy fontawesome-24v0.
           destroy fontawesome-24v00.
       is-destroy-bitmaps.
           call "w$bitmap" using wbitmap-destroy abc-jpg.
           call "w$bitmap" using wbitmap-destroy bakgrunn-jpg0.
           call "w$bitmap" using wbitmap-destroy button-jpg.
           call "w$bitmap" using wbitmap-destroy button-png.
       is-destroy-menus.
           call "w$menu" using wmenu-destroy hmenu.

       is-screen-1-routine.
           perform is-screen-1-scrn
           perform is-screen-1-proc.
       is-screen-1-scrn.
           perform is-screen-1-create
           perform is-screen-1-init-data.
       is-screen-1-create.
           perform screen-1-bef-create.
           display initial window
              screen line 41
              screen column 91
              size 64,0
              lines 43,0
              cell width 10
              cell height 10
              label-offset 20
              background-color rgb x#ffffff foreground-color rgb 
              x#000000
              background-color rgb x#ffffff
              resizable
              modeless
              title-bar
              auto-minimize
              no wrap
              title "Pålogging"
              layout-manager lm-scale
              gradient-orientation 0
           .
           display tool-bar multiline
              cell width 10
              cell height 10
              control font Default-Font
              lines 8,9
              handle screen-1-tb-1-hdl
           .
           display screen-1-tb-1 upon screen-1-tb-1-hdl.
           display status-bar
              font Default-Font
              color 257
              foreground-color rgb x#004080
              panel-widths ( 10)
              panel-style ( 1)
              panel-alignment ( "C")
              panel-bitmap ( 0)
              panel-bitmap-width ( 0)
              panel-bitmap-number ( 0)
              panel-bitmap-alignment ( "U")
              id 12
              handle screen-1-st-1-hdl
           .
           modify screen-1-st-1-hdl
              panel-index 1
              panel-text "teste"
           .

           display screen-1.
           perform screen-1-aft-create.
       is-screen-1-init-data.
           modify screen-1-tc-1
              tab-to-add ( "Page-1" "Page-2")
              bitmap-number ( 0 0)
           .
           perform is-screen-1-lb-1-content.
           perform is-screen-1-gr-1-content.
           call "w$scale" using abc-jpg 37 33 giving abc-jpg.
           modify screen-1-pb-6 bitmap-handle abc-jpg.
           perform screen-1-aft-init-data.
       is-screen-1-lb-1-content.
           modify screen-1-lb-1
              dividers ( 1 )
              separation ( 5 )
              alignment ( "U" )
           .
           modify screen-1-lb-1
              row-background-color-pattern rgb 16777215 
              row-foreground-color-pattern rgb 0
              row-background-color-pattern rgb 15594236 
              row-foreground-color-pattern rgb 0
           .
           modify screen-1-lb-1
                 item-to-add "item"
                 item-to-add "item"
                 item-to-add "item"
           .
       is-screen-1-gr-1-content.
           modify screen-1-gr-1
              column-dividers ( 1 1 1 1 1 )
              data-columns ( 1 9 17 25 33 )
              display-columns ( 1 9 17 25 33 )
              separation ( 5 5 5 5 5 )
              alignment ( "U" "U" "U" "U" "U" )
              data-types ( "X" "X" "X" "X" "X" )
           .
       is-screen-1-display-status-msg.
           modify screen-1-st-1-hdl
              font Default-Font
              color 257
              foreground-color rgb x#004080
              panel-widths ( 10)
              panel-style ( 1)
              panel-alignment ( "C")
              panel-bitmap ( 0)
              panel-bitmap-width ( 0)
              panel-bitmap-number ( 0)
              panel-bitmap-alignment ( "U")
           .
           modify screen-1-st-1-hdl
              panel-index 1
              panel-text "teste"
           .
       is-screen-1-clear-status-msg.
           perform is-screen-1-display-status-msg.
       is-screen-1-proc.
           perform until exit-pushed
              accept screen-1 on exception 
                 perform is-screen-1-evaluate-func
              end-accept
           end-perform.
           initialize key-status.
       is-screen-1-evaluate-func.
           evaluate true
           when exit-pushed
              perform is-screen-1-exit
           when event-occurred
              if event-type = msg-close
                 perform is-screen-1-exit
              end-if
           when key-status = 1006
              perform screen-1-pb-3-link-to
              when key-status = 1005
                 perform screen-1-mn-1-link-to
           end-evaluate.
           move 1 to accept-control.
       is-screen-1-exit.
           set exit-pushed to true.
       is-scrMain-routine.
           perform is-scrMain-scrn
           perform is-scrMain-proc.
       is-scrMain-scrn.
           perform is-scrMain-create
           perform is-scrMain-init-data.
       is-scrMain-create.
           display standard graphical window background-low
              screen line 41
              screen column 91
              size 150,7
              lines 96,9
              cell width 10
              cell height 10
              label-offset 20
              color 257
              resizable
              modeless
              title-bar
              auto-minimize
              no wrap
              title appl-title
              layout-manager lm-scale
              handle main-window
           .
           move bakgrunn-jpg0 to is-scrMain-bkg-img-tmp.
           call "w$scale" using bakgrunn-jpg0 1507 969 giving 
           bakgrunn-jpg0.
           modify is-scrMain-bkg-img bitmap-handle bakgrunn-jpg0.
           call "w$bitmap" using wbitmap-destroy is-scrMain-bkg-img-tmp.
           display scrMain.
       is-scrMain-init-data.
           perform scrMain-aft-init-data.
       is-scrMain-proc.
           perform until exit-pushed
              accept omitted on exception 
                 perform is-scrMain-evaluate-func
              end-accept
           end-perform.
           destroy main-window.
           initialize key-status.
       is-scrMain-evaluate-func.
           evaluate true
           when exit-pushed
              perform is-scrMain-exit
           when event-occurred
              if event-type = msg-close
                 perform is-scrMain-exit
              end-if
              if event-type = ntf-resized
                 inquire main-window size in is-scrMain-bkg-img-size
                 inquire main-window lines in is-scrMain-bkg-img-lines
                 move bakgrunn-jpg0 to is-scrMain-bkg-img-tmp
                 call "w$scale" using bakgrunn-jpg0 
                 is-scrMain-bkg-img-size is-scrMain-bkg-img-lines 
                 main-window giving bakgrunn-jpg0
                 call "w$bitmap" using wbitmap-destroy 
                 is-scrMain-bkg-img-tmp
                 display is-scrMain-bkg-img
              end-if
           end-evaluate.
           move 1 to accept-control.
       is-scrMain-exit.
           set exit-pushed to true.
       is-create-screen-1-mn-1.
           perform is-screen-1-mn-1 thru is-screen-1-mn-1-exit.
       is-screen-1-mn-1.
           call "w$menu" using wmenu-new giving menu-handle.
           if menu-handle = zero
              go to is-screen-1-mn-1-exit
           end-if 
           call "w$menu" using wmenu-new giving sub-handle-1.
           if sub-handle-1 = zero
              move zero to menu-handle
              go to is-screen-1-mn-1-exit
           end-if 
           call "w$menu" using wmenu-add menu-handle 0 0 "1" 1000 
           sub-handle-1.
           call "w$menu" using wmenu-add sub-handle-1 0 0 "11" 1002.
           call "w$menu" using wmenu-add sub-handle-1 0 256.
           call "w$menu" using wmenu-add sub-handle-1 0 0 "13" 1004.
           call "w$menu" using wmenu-new giving sub-handle-1.
           if sub-handle-1 = zero
              move zero to menu-handle
              go to is-screen-1-mn-1-exit
           end-if 
           call "w$menu" using wmenu-add menu-handle 0 0 "2" 1001 
           sub-handle-1.
           call "w$menu" using wmenu-add sub-handle-1 0 0 "21" 1005.
       is-screen-1-mn-1-exit.
           move zero to return-code.
       screen-1-pb-1-evt-proc.
           evaluate event-type
           when cmd-goto
              perform screen-1-pb-1-evt-cmd-goto
           when other
           end-evaluate
           .
      *start event editor code

       screen-1-bef-create.
           move 78-link to but-title.
           move nbtn-title to btn-title
           move 78-calc to but-title.
           move nbtn-title to btn1-title           
           .
       screen-1-pb-1-evt-cmd-goto.
           display message "hi" 
           .
       scrMain-aft-init-data.
           modify is-scrMain-bkg-img layout-data rlm-resize-both-any  
           .
       screen-1-aft-init-data.
      *     modify button-jpg layout-data rlm-move-both-any  
           .

       screen-1-mn-1-link-to.
           display message "hi again" 
           .
       screen-1-aft-create.
           perform is-create-screen-1-mn-1 
           .
       screen-1-pb-3-link-to.
           display message "hello"
           modify screen-1-pb-2 title "testes"
           modify screen-1-pb-2 background-color, rgb=x#0E768C 
           .
      *end {iscobol}copy-procedure
      *begin {iscobol}external-copyfiles
      *end {iscobol}external-copyfiles
