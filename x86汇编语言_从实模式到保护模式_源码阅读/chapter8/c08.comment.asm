         ;代码清单8-2
         ;文件名：c08.asm
         ;文件说明：用户程序 
         ;创建日期：2011-5-5 18:17
         
;===============================================================================

; // 程序头部在源程序中以一个段的形式出现, 用户程序头部起码包含以下信息:
; // ① 用户程序的尺寸, 单位字节, 即双字 dd ( 程序长度可能 > 65535, 16 位长度 dw 不足表示, 所以用32位 dd )变量 program_length, 值是 program_end, 编译阶段, 编译器会把标号 program_end 的汇编地址填写在这里; program_end 属于段 trail, 而段 trail 没有 vstart 语句, 所以 program_end 的汇编地址是从整个程序开头计算的,数值上等于整个程序的长度

; // ② 程序的入口点, 即变量 code_entry, 包括段地址和偏移地址, 由于 program_length 是双字类型 dd 变量，一个双字数据占4个字节单元，读完一个，偏移量加4, 所以 "code_entry dw start" 的偏移地址是 0x04
; // 以此类推, dw定义字类型变量，一个字数据占2个字节单元，读完一个，偏移量加2, dd section.code_1.start 的偏移地址是 0x06
; // 程序的入口点偏移地址是代码段 code_1 的标号 start, 因为 code_1 段定义包含了 vstart=0, 所以 start 代表的汇编地址是相对 code_1 的起始位置, 从0开始计算的
; // 段地址是 code_1 的地址, 用表达式 section.code_1.start 来表示

; // ③ 段重定位表; 段的重定位是加载器的工作,需要知道每个段在用户程序中的位置,即它们分别位于用户程序内的多少字节处
; // 由于段的个数是不确定的,所以用标号 header_end 的地址减去 code_1_segment 的地址除以 4, 来确定有多少个表项, 即表达式 (header_end-code_1_segment)/4

; // vstart=0 表示引用 header 段内的标号时,标号处的汇编地址是从 header 段地址开始计算; 如果没有 vstart=0, 标号出的汇编地址是从整个程序的开头计算
SECTION header vstart=0                     ;定义用户程序头部段 
    program_length  dd program_end          ;程序总长度[0x00]
    
    ;用户程序入口点
    code_entry      dw start                ;偏移地址[0x04]
                    dd section.code_1.start ;段地址[0x06] 
    
    realloc_tbl_len dw (header_end-code_1_segment)/4
                                            ;段重定位表项个数[0x0a]
    
    ;段重定位表           ZQZQz
    code_1_segment  dd section.code_1.start ;[0x0c]
    code_2_segment  dd section.code_2.start ;[0x10]
    data_1_segment  dd section.data_1.start ;[0x14]
    data_2_segment  dd section.data_2.start ;[0x18]
    stack_segment   dd section.stack.start  ;[0x1c]
    
    header_end:                
    
;===============================================================================
; // align=16 表示引用 code_1 段在内存中的汇编地址是16字节对齐,是16的倍数,或者说 code_1 的物理地址能被 16 整除
SECTION code_1 align=16 vstart=0         ;定义代码段1（16字节对齐） 
put_string:                              ;显示串(0结尾)。
                                         ;输入：DS:BX=串地址										 
		; // 字符串是 db 0 结尾,所以这里判断 cl = 0 ?
         mov cl,[bx]
         or cl,cl                        ;cl=0 ?
         jz .exit                        ;是的，返回主程序 
         call put_char
         inc bx                          ;下一个字符 
         jmp put_string

   .exit:
         ret

;-------------------------------------------------------------------------------
put_char:                                ;显示一个字符
                                         ;输入：cl=字符ascii
         push ax
         push bx
         push cx
         push dx
         push ds
         push es

         ;以下取当前光标位置
         mov dx,0x3d4
         mov al,0x0e
         out dx,al
         mov dx,0x3d5
         in al,dx                        ;高8位 
         mov ah,al

         mov dx,0x3d4
         mov al,0x0f
         out dx,al
         mov dx,0x3d5
         in al,dx                        ;低8位 
         mov bx,ax                       ;BX=代表光标位置的16位数

         cmp cl,0x0d                     ;回车符？
         jnz .put_0a                     ;不是。看看是不是换行等字符 
         mov ax,bx                       ;此句略显多余，但去掉后还得改书，麻烦 
         mov bl,80                       
         div bl
         mul bl
         mov bx,ax
         jmp .set_cursor

 .put_0a:
         cmp cl,0x0a                     ;换行符？
         jnz .put_other                  ;不是，那就正常显示字符 
         add bx,80
         jmp .roll_screen

 .put_other:                             ;正常显示字符
         mov ax,0xb800
         mov es,ax
         shl bx,1
         mov [es:bx],cl

         ;以下将光标位置推进一个字符
         shr bx,1
         add bx,1

 .roll_screen:
         cmp bx,2000                     ;光标超出屏幕？滚屏
         jl .set_cursor

         mov ax,0xb800
         mov ds,ax
         mov es,ax
         cld
         mov si,0xa0
         mov di,0x00
         mov cx,1920
         rep movsw
         mov bx,3840                     ;清除屏幕最底一行
         mov cx,80
 .cls:
         mov word[es:bx],0x0720
         add bx,2
         loop .cls

         mov bx,1920

 .set_cursor:
         mov dx,0x3d4
         mov al,0x0e
         out dx,al
         mov dx,0x3d5
         mov al,bh
         out dx,al
         mov dx,0x3d4
         mov al,0x0f
         out dx,al
         mov dx,0x3d5
         mov al,bl
         out dx,al

         pop es
         pop ds
         pop dx
         pop cx
         pop bx
         pop ax

         ret

;-------------------------------------------------------------------------------
  start:
         ;初始执行时，DS和ES指向用户程序头部段
         mov ax,[stack_segment]           ;设置到用户程序自己的堆栈 
         mov ss,ax
         mov sp,stack_end
         
         mov ax,[data_1_segment]          ;设置到用户程序自己的数据段
         mov ds,ax

		 ; // 设置 ds 为 data_1, bx 为 msg0 
         mov bx,msg0
         call put_string                  ;显示第一段信息 

		 ; // 压入 code_2 段地址 和 偏移地址
         push word [es:code_2_segment]
         mov ax,begin
         push ax                          ;可以直接push begin,80386+
         
		 ; // 从栈中将偏移地址和段地址取出,赋值给 cs:ip
         retf                             ;转移到代码段2执行 
         
  continue:
         mov ax,[es:data_2_segment]       ;段寄存器DS切换到数据段2 
         mov ds,ax
         
         mov bx,msg1
         call put_string                  ;显示第二段信息 

		 ; // 无限循环
         jmp $ 

;===============================================================================
SECTION code_2 align=16 vstart=0          ;定义代码段2（16字节对齐）

  begin:
         push word [es:code_1_segment]
         mov ax,continue
         push ax                          ;可以直接push continue,80386+
         
         retf                             ;转移到代码段1接着执行 
         
;===============================================================================
SECTION data_1 align=16 vstart=0
;// 用变量 msg0 来引用下面一大段字符
    msg0 db '  This is NASM - the famous Netwide Assembler. '
         db 'Back at SourceForge and in intensive development! '
         db 'Get the current versions from http://www.nasm.us/.'
         db 0x0d,0x0a,0x0d,0x0a
         db '  Example code for calculate 1+2+...+1000:',0x0d,0x0a,0x0d,0x0a
         db '     xor dx,dx',0x0d,0x0a
         db '     xor ax,ax',0x0d,0x0a
         db '     xor cx,cx',0x0d,0x0a
         db '  @@:',0x0d,0x0a
         db '     inc cx',0x0d,0x0a
         db '     add ax,cx',0x0d,0x0a
         db '     adc dx,0',0x0d,0x0a
         db '     inc cx',0x0d,0x0a
         db '     cmp cx,1000',0x0d,0x0a
         db '     jle @@',0x0d,0x0a
         db '     ... ...(Some other codes)',0x0d,0x0a,0x0d,0x0a
         db 0

;===============================================================================
SECTION data_2 align=16 vstart=0

    msg1 db '  The above contents is written by LeeChung. '
         db '2011-05-06'
         db 0

;===============================================================================
SECTION stack align=16 vstart=0
           ; // 从当前位置开始,保留 256 个字节的栈空间,但不初始化它们的值
         resb 256
		; // 栈段 stack vstart=0, 所 stack_end 的汇编地址是 256
stack_end:  

;===============================================================================
SECTION trail align=16
program_end: