; An unexpected blizzard, Single Screen Demo
; by Ref 29.12.2018 for RAAT#05, Istanbul/Turkey

; Kar8.scr Image colors are optimized for black&white TV
; Use grey palette when watching this little demo

; compile with pasmo -v --tapbas unexpectedb.asm unexpectedb.tap
; sorry for the mixed language comments, they are in Turkish

satirbasi equ 16384+25 ;scroller satýr adresi
snow_count equ 120     ;kac adet kar olacak?
slow equ snow_count-90  ;yavaþ yagan kar sayisi
med equ slow-10    ;orta hizda yaganlar    (hiz=2)
hi equ med-5    ;deli gibi yaganlar       (hiz=3)
sintab2 equ &8B00
sintab3 equ &8C00
locxy equ &8E00
font equ &9600

break_call_scroller equ 70  ;yað gibi bir kayma için bu rakam 60k-70k ts arasýna denk gelmeli
org 32768

ld sp,65535
di
ld a,0
out (254),a

; --- align the sin tables ---
call align_tables

; --- initalize audio ---
          ld      d,7             ; select the mixer register
          ld      e,63             ; disble all
          call    outer           ; send it to PSG

          ld      d,13            ; select the envelope shape register
          ld      e,14            ; repeat Attack-Decay
          call    outer           ; send it to PSG

          ld      d,9             ; channel B volume
          ld      e,1             ; minimum
          call    outer           ; send it to PSG
; --- done ---



ld ix,yazi             ;metnimizi index registerine yüklüyoruz

; --- setup snowflake table ---
ld c,snow_count+1     ;we'll be preparing for snow flakes
ld de,locxy           ;generate snow data at this location
ld a,snow_count
ld (buffer),a

Loop1:
ld a,c
cp slow
jp nc, newy
cp hi
jp nc, newyb
ld a,3;3
jp newywrite
newyb:
ld a,2;2
jp newywrite
newy:
ld a,1;1
newywrite:
ld (de),a ; Y speed of the flake
inc de

getnumby:   ;get a y coord of the flake
call random
cp 170
jr nc, getnumby
add a,8
ld (de),a
inc de

getnumbx:    ;get a x coord
call random
cp 128
jr nc, getnumbx
add a, 48
ld (de),a
inc de

getnumbs:   ;get a sintab index so it behaves different than others
call random
cp 63
jr nc, getnumbs
ld (de),a
inc de

getnumbt:   ;get a second sintab index for further differentiate the flake
call random
cp 63
jr nc, getnumbt
ld (de),a

inc de ;leave three byte space for cleanup buffer, default it to an unused address.
;addressHI
ld a,&40
ld (de),a
inc de
;addressLO
ld a,0
ld (de),a
inc de
;bufferData
ld a,0
ld (de),a

inc de

dec c
jr nz,Loop1

; -- Snowflake table is ready --

;Load the image
 ld hl,&c000
 ld de,&4000
 ld bc,6912
 ldir

; set up interrupts
;(Shamelessly taken from Gasman's Confetti)
ld hl,0xbe00
ld de,0xbe01
ld (hl),0xbf
ld bc,0x0101
ldir
ld a,0xc3 ; JP
ld (0xbfbf),a
ld hl,interrupt
ld (0xbfc0),hl

ld a,0xbe
ld i,a
im 2
ei
; -- ok, we are ready! set up the main loop and leave ---

main_loop
halt   ;this will call the interrupt routine every 50th of a second
jr main_loop


interrupt

ld de,locxy    ;kar index=0

loop2:

ex de,hl
ld a,(hl) ;get Y speed

inc hl
ld c,(hl)     ;get the y value

add a,c ;increase Y
cp 182  ;if Y>182
jr c,cont  ;then
ld c,8      ;Y=0

call getnumbx2
inc hl
ld (hl),a
dec hl

jr cont2
cont:
ld c,a       ;Y=a
cont2:
ld (hl),c

inc hl
ld b,(hl)     ;get the x value

ld a,c
cp 40    ; only y>40 is affected from blizzard
jp c,no_blizzard

;now Y>40
push hl ;check if there is a blizzard now
ld hl,(timer1)
ld a,h

ld a,b
or a          ;reset carry
rr h          ;divide by 2
sub h         ;modify x position
rl h          ;multiply back
ld b,a        ;record new position

ld a,c
cp 130   ;if y>130 they are affected more from blizzard
jp c,last_blizzard
ld a,b
sub h
ld b,a

last_blizzard:
ld a,b
cp 40         ;check if this pixel goes out of screen
jp nc, write_blizzard
;reset x position
ld b,200
write_blizzard
pop hl
ld (hl),b
no_blizzard:

inc hl

ld a,(hl)     ;get sintab1 displacement
inc a
cp 63
jr c,cont3
ld a,0
ld (hl),a
cont3:
ld (hl),a
push hl
push bc
ld c,a
ld b,0

ld hl,sintab2
add hl,bc
ld a,(hl)
pop bc
add a,b
ld b,a
pop hl

inc hl

ld a,(hl)     ;get sintab2 displacement
inc a         ;increase index
cp 63
jr c,cont4
ld a,0
ld (hl),a
cont4:
ld (hl),a     ;put it in if it's ok
push hl
push bc
ld c,a
ld b,0

ld hl,sintab3 ;get table data
add hl,bc
ld a,(hl)     ;get displacement value
pop bc
add a,b       ;apply value
ld b,a

pop hl

inc hl
push bc
;put backbuffer here
ld b,(hl)
inc hl
ld c,(hl)
inc hl
ld a,(hl)
ld (bc),a ;restore original data on screen
pop bc
dec hl
dec hl ;data pointer points to 3 byte backbuffer now

ex de,hl     ;DE is now locxy

call Calculate_Pixel_Address ;get the address and pixel

              ;now HL holds screen address, a holds pixel position
              ;DE still holds locxy snowflake table index


ex af,af'     ;backup pixel position
ld bc,&8000
add hl,bc     ;jump to clean image address
ld a,(hl)     ;gather clean image data to a

sbc hl,bc     ;back to screen position

ex de,hl      ;hl now holds locxy, while hl screen address

ld (hl),d     ;write hl and a to 3byte buffer
inc hl
ld (hl),e
inc hl
ld (hl),a    ;put clean image data to buffer

ex de, hl    ;de is locxy again
ex af,af'    ;a holds pixel position again

;now find the pixel pattern from pixel table
LD BC,Plot_Point    ; Address of point lookup table
ADD A,C             ; Add pixel position to get entry in table
LD C,A
LD A,(BC)           ; Get pixel data from table
or(HL)              ; OR with screen data
LD (HL),A           ; Write back to screen

inc de              ; move pointer to next locxy data

ld a,(buffer)
cp break_call_scroller  ;this makes scroller more fluid
call z, scroller        ;let's take a break on snow flakes and scroll text one pixel

ld a,(buffer)
dec a
ld (buffer),a

jp nz,loop2

ld a,snow_count
ld (buffer),a

;slowly increase snow count over time
;ld a,(snowdef)
;inc a
;cp snow_count
;jr nz,update_buffer
;dec a
;update_buffer
;ld (snowdef),a
;ld (buffer),a

ld hl,(timer1)
self_modify inc hl
ld (timer1),hl
ld a,l
cp 255
call z,adjust_noise ;increase noise volume with blizzard


;before going back lets check if there is a blizzard going on
ld a,h ;get the highbyte value
cp 5  ;check if blizzard has ended, and reset the counter
jp nz,go_blizz
;coll blizz down
ld hl,self_modify
ld (hl),&2B    ;write DEC HL
go_blizz
cp 0
jp nz, more_blizz
;increase blizz
ld hl,self_modify
ld (hl),&23    ;write INC HL
more_blizz

call scroller
ei
ret


really_no_blizzard
pop hl
jp no_blizzard

getnumbx2:    ;get a new x coord
call random
cp 128
jr nc, getnumbx2
add a, 48
ret

scroller:

push de

;paint a couple of blocks for a smoother color gradient for scroller
ld hl,flip
ld a,(hl)
xor 1
ld (hl),a
jp z, paintblocks
ld a, &3d
ld (22538),a
ld (22549),a
ld (22537),a
ld (22550),a
jp initscroll
paintblocks
ld a, &3c
ld (22538),a
ld (22549),a
ld (22537),a
ld (22550),a

initscroll

ld de,#4019            ;karakterin pozisyonu
xor a
sekizkaydir

ld hl,scrole
ld e,(hl)
basadon

ld b,8
ld hl, satirbasi     ;HL'yi satirbasi yap
dondur

rept 19               ;33 kere tekrar (pasmo macro)
 rl(hl)
 dec l
endm

inc h                 ;alt satýra geç
ld a,19               ;satir sonuna sardýr
add a,l
ld l,a

dec b
jp nz, dondur           ;eger 8 satirin her birini islemediysek tekrar kaydir

ld hl,scrole
dec e                 ;kac kere kaydirdigimizi say
ld (hl),e

jp nz,break_scroll
;jp nz, basadon        ;eger 8 kere sola kaydirdiysak, yeni bir karakter basmamiz gerekecek

inc ix                ;indeksi bir arttir
call harfibas           ; CALL yeni harfi bas
;ret
ld hl,scrole
ld(hl),8

break_scroll      ;Scroller burda biter


pop de

ret


; Burada ana döngü bitiyor, harf yazdýrma altrutini burada:

harfibas                ;tek bir karakter basmak icin cok ugrasiyorum sanki
ld de,#4019             ;hep ayný lokasyona basýyoruz
ld a,(ix+0)		;diziden yeni bir karakter alalim
or a                    ;a 0 ise Z flag reset edilir.
JR NZ,jump1             ;string sonu deðil ise jump1'den devam
                        ;yoksa, sýfýrý bulduk, string sonu
ld ix,yazi              ;baþa alalým
jr harfibas

jump1
ld bc, font-256		;BC'ye ROM font adresini yüklerim #3c00
ld l,a                  ;karakter kodunu kullanarak font'un o karakterine ulaþacaðým
ld h,0

add hl,hl		;HL=HL*8 karakterin ROM'daki pozisyonuna ulaþmak için
add hl,hl               ;gerekli olan hesap :)
add hl,hl               ;ne demiþler, çarpma, hýzlýca toplamak demek :)

add hl,bc		;ve nihayet HL doðru font verisinin ilk byte'ýný gösteriyor

ld b,#08                ;8 byte kopyalayacaðýz

ekranakoy
ld a,(hl)		;fontun ilk byte'ýný alalým
ld (de),a               ;ilk byte'ý ekrana yazalým

inc l                   ;HL ikinci byte'ý göstersin
inc d                   ;DE ikinci satýra geçsin (speccy ekran kaþesi bu iþlem için tasarlanmýþ,
                        ;aþaðý/yukarý hareket etmek için hibyte'ý deðiþtirmek yetiyor
djnz ekranakoy          ;bu iþlem b=0 olana kadar tekrar edecek

ret                  ;bitti. yallah



yazi db " ~ Hello scene! Today I am presenting to you a black and white tv special: 'an Unexpected Blizzard', Gfx & Code & Noise by Ref. This little single screen demo was conjured a couple of days before the party and released at RAAT#05, 29.Dec.2018, Istanbul. RAAT is a mini gathering of the few sceners left in Turkey. You are welcome to join us at retrojen.org > Greets and respects goes to: Cheveron, Gasman, Dunny, Marko, Woody, Gemba, HooyProg, Atebit Paul, inward, skrju, 4mat, diver and factor5.~ RAAT#05 Parti sonras| introsu! Ka< pixel var burda? 60! Yok yok 75... 50fps efekt, iki sin}s tablosu, 2k'l|k bir tampon! F|rt|na ve r}zgar! WR! WR! $aka $aka, not wr :) Hemen herkese harf s|ras|na g{re selamlar: Alco _ Shax _ Domino _ Adam|n Biri _ Madcat _ Gaddar _ Wisdom _ Impetigo _ Beast _ Zero _ Hydrogen _ matahari _ Eins _ Skate _ hades _ Nightlord _ Akermen _ Curt _ ssg _ alpyre _ norvax _ Wizofwor _ Endo _ Fero _ Blockmind _ ilky _ function _ Caisson _ Arcane _ Witchdoctor _ Pe@cer _ Atillan _ emarti _ coze _ Amonr _ ilkerg _ cengizermis _ Hem isimlerin ne {nemi var?    EoF.            ",0






; Get screen address (J.Claudwell)

; On Entry: B reg = X coord,  C reg = Y coord
; On Exit: HL = screen address, A = pixel position

Calculate_Pixel_Address:
; Calculate the high byte of the screen addressand store in H reg.

	ld a,c
	and %00000111
	ld h,a
	ld a,c
	rra
	rra
	rra
	and %00011000
	or h
	or %01000000
	ld h,a

; Calculate the low byte of the screen address and store in L reg.

	ld a,b
	rra
	rra
	rra
	and %00011111
	ld l,a
	ld a,c
	rla
	rla
	and %11100000
	or l
	ld l,a

; Calculate pixel postion and store in A reg.

	ld a,b
	and %00000111

 ret

;random code is from a random site: http://wikiti.brandonw.net/index.php?title=Z80_Routines:Math:Random
random:
        push    hl
        push    de
        ld      hl,(randData)
        ld      a,r
        ld      d,a
        ld      e,(hl)
        add     hl,de
        add     a,l
        xor     h
        ld      (randData),hl
        pop     de
        pop     hl
ret


adjust_noise

            push bc
            push de


            ld      e,0             ; enable all
            ld a,h
            or a
            jp nz, ay_end
            ld      e,63            ; disable all

            ay_end
            ld      d,7             ; select the mixer register
            call    outer           ; send it to PSG

            ld      d,6             ; channel A noise pitch
            ld      e,h             ; pitch value
            call    outer           ; send it to PSG

            ld      d,9             ; channel B volume
            ld      e,1             ; minimum
            call    outer           ; send it to PSG


            pop de
            pop bc
ret

;AY Music chip control function taken from http://www.armory.com/~rstevew/Public/SoundSynth/Novelty/AY3-8910/start.html
ayctrl  EQU     65533
aydata  EQU     49149

outer
        ld      bc,ayctrl               ; select control port
        out     (c),d           ; send specified value
        ld      bc,aydata       ; select data port
        out     (c),e           ; send specified value
        ret

;align all data ro 256byte boundary
align_tables

        ld hl,image
        ld de,&c000
        ld bc,6912
        ldir

        ld hl,sintab2d
        ld de,&8b00
        ld bc,64
        ldir

        ld hl,sintab3d
        ld de,&8c00
        ld bc,64
        ldir

        ld hl,fontd
        ld de,&9600
        ld bc,768
        ldir
ret

;org &8A00
;sintab1 defb 8,9,10,10,11,12,12,13,14,14,15,15,15,16,16,16,16,16,16,16,15,15,15,14,14,13,12,12,11,10,10,9,8,7,6,6,5,4,4,3,2,2,1,1,1,0,0,0,0,0,0,0,1,1,1,2,2,3,4,4,5,6,6,7
fontd incbin "TopHeavyTR.specchr"
sintab2d defb 16,18,19,21,22,24,25,26,27,28,29,30,31,31,32,32,32,32,32,31,31,30,29,28,27,26,25,24,22,21,19,18,16,14,13,11,10,8,7,6,5,4,3,2,1,1,0,0,0,0,0,1,1,2,3,4,5,6,7,8,10,11,13,14
sintab3d defb 4,5,6,6,7,7,8,8,8,8,8,7,7,6,6,5,4,3,2,2,1,1,0,0,0,0,0,1,1,2,2,3,4,5,6,6,7,7,8,8,8,8,8,7,7,6,6,5,4,3,2,2,1,1,0,0,0,0,0,1,1,2,2,3
Plot_Point:     DB %10000000,%01000000,%00100000,%00010000,%00001000,%00000100,%00000010,%00000001
buffer   defb 0
snowdef  defb 1
lastpix  defb 0
randData defw 5
timer1   defw 0
timer2   defw 0
blizzx   defb 1
flip     defb 0
scrole   defb 0
image incbin "kar8.scr"
end 32768
