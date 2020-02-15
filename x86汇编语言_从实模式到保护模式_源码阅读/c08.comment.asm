         ;�����嵥8-2
         ;�ļ�����c08.asm
         ;�ļ�˵�����û����� 
         ;�������ڣ�2011-5-5 18:17
         
;===============================================================================

; // ����ͷ����Դ��������һ���ε���ʽ����, �û�����ͷ���������������Ϣ:
; // �� �û�����ĳߴ�, ��λ�ֽ�, ��˫�� dd ( ���򳤶ȿ��� > 65535, 16 λ���� dw �����ʾ, ������32λ dd )���� program_length, ֵ�� program_end, ����׶�, ��������ѱ�� program_end �Ļ���ַ��д������; program_end ���ڶ� trail, ���� trail û�� vstart ���, ���� program_end �Ļ���ַ�Ǵ���������ͷ�����,��ֵ�ϵ�����������ĳ���

; // �� �������ڵ�, ������ code_entry, �����ε�ַ��ƫ�Ƶ�ַ, ���� program_length ��˫������ dd ������һ��˫������ռ4���ֽڵ�Ԫ������һ����ƫ������4, ���� "code_entry dw start" ��ƫ�Ƶ�ַ�� 0x04
; // �Դ�����, dw���������ͱ�����һ��������ռ2���ֽڵ�Ԫ������һ����ƫ������2, dd section.code_1.start ��ƫ�Ƶ�ַ�� 0x06
; // �������ڵ�ƫ�Ƶ�ַ�Ǵ���� code_1 �ı�� start, ��Ϊ code_1 �ζ�������� vstart=0, ���� start ����Ļ���ַ����� code_1 ����ʼλ��, ��0��ʼ�����
; // �ε�ַ�� code_1 �ĵ�ַ, �ñ��ʽ section.code_1.start ����ʾ

; // �� ���ض�λ��; �ε��ض�λ�Ǽ������Ĺ���,��Ҫ֪��ÿ�������û������е�λ��,�����Ƿֱ�λ���û������ڵĶ����ֽڴ�
; // ���ڶεĸ����ǲ�ȷ����,�����ñ�� header_end �ĵ�ַ��ȥ code_1_segment �ĵ�ַ���� 4, ��ȷ���ж��ٸ�����, �����ʽ (header_end-code_1_segment)/4

; // vstart=0 ��ʾ���� header ���ڵı��ʱ,��Ŵ��Ļ���ַ�Ǵ� header �ε�ַ��ʼ����; ���û�� vstart=0, ��ų��Ļ���ַ�Ǵ���������Ŀ�ͷ����
SECTION header vstart=0                     ;�����û�����ͷ���� 
    program_length  dd program_end          ;�����ܳ���[0x00]
    
    ;�û�������ڵ�
    code_entry      dw start                ;ƫ�Ƶ�ַ[0x04]
                    dd section.code_1.start ;�ε�ַ[0x06] 
    
    realloc_tbl_len dw (header_end-code_1_segment)/4
                                            ;���ض�λ�������[0x0a]
    
    ;���ض�λ��           ZQZQz
    code_1_segment  dd section.code_1.start ;[0x0c]
    code_2_segment  dd section.code_2.start ;[0x10]
    data_1_segment  dd section.data_1.start ;[0x14]
    data_2_segment  dd section.data_2.start ;[0x18]
    stack_segment   dd section.stack.start  ;[0x1c]
    
    header_end:                
    
;===============================================================================
; // align=16 ��ʾ���� code_1 �����ڴ��еĻ���ַ��16�ֽڶ���,��16�ı���,����˵ code_1 �������ַ�ܱ� 16 ����
SECTION code_1 align=16 vstart=0         ;��������1��16�ֽڶ��룩 
put_string:                              ;��ʾ��(0��β)��
                                         ;���룺DS:BX=����ַ										 
		; // �ַ����� db 0 ��β,���������ж� cl = 0 ?
         mov cl,[bx]
         or cl,cl                        ;cl=0 ?
         jz .exit                        ;�ǵģ����������� 
         call put_char
         inc bx                          ;��һ���ַ� 
         jmp put_string

   .exit:
         ret

;-------------------------------------------------------------------------------
put_char:                                ;��ʾһ���ַ�
                                         ;���룺cl=�ַ�ascii
         push ax
         push bx
         push cx
         push dx
         push ds
         push es

         ;����ȡ��ǰ���λ��
         mov dx,0x3d4
         mov al,0x0e
         out dx,al
         mov dx,0x3d5
         in al,dx                        ;��8λ 
         mov ah,al

         mov dx,0x3d4
         mov al,0x0f
         out dx,al
         mov dx,0x3d5
         in al,dx                        ;��8λ 
         mov bx,ax                       ;BX=������λ�õ�16λ��

         cmp cl,0x0d                     ;�س�����
         jnz .put_0a                     ;���ǡ������ǲ��ǻ��е��ַ� 
         mov ax,bx                       ;�˾����Զ��࣬��ȥ���󻹵ø��飬�鷳 
         mov bl,80                       
         div bl
         mul bl
         mov bx,ax
         jmp .set_cursor

 .put_0a:
         cmp cl,0x0a                     ;���з���
         jnz .put_other                  ;���ǣ��Ǿ�������ʾ�ַ� 
         add bx,80
         jmp .roll_screen

 .put_other:                             ;������ʾ�ַ�
         mov ax,0xb800
         mov es,ax
         shl bx,1
         mov [es:bx],cl

         ;���½����λ���ƽ�һ���ַ�
         shr bx,1
         add bx,1

 .roll_screen:
         cmp bx,2000                     ;��곬����Ļ������
         jl .set_cursor

         mov ax,0xb800
         mov ds,ax
         mov es,ax
         cld
         mov si,0xa0
         mov di,0x00
         mov cx,1920
         rep movsw
         mov bx,3840                     ;�����Ļ���һ��
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
         ;��ʼִ��ʱ��DS��ESָ���û�����ͷ����
         mov ax,[stack_segment]           ;���õ��û������Լ��Ķ�ջ 
         mov ss,ax
         mov sp,stack_end
         
         mov ax,[data_1_segment]          ;���õ��û������Լ������ݶ�
         mov ds,ax

		 ; // ���� ds Ϊ data_1, bx Ϊ msg0 
         mov bx,msg0
         call put_string                  ;��ʾ��һ����Ϣ 

		 ; // ѹ�� code_2 �ε�ַ �� ƫ�Ƶ�ַ
         push word [es:code_2_segment]
         mov ax,begin
         push ax                          ;����ֱ��push begin,80386+
         
		 ; // ��ջ�н�ƫ�Ƶ�ַ�Ͷε�ַȡ��,��ֵ�� cs:ip
         retf                             ;ת�Ƶ������2ִ�� 
         
  continue:
         mov ax,[es:data_2_segment]       ;�μĴ���DS�л������ݶ�2 
         mov ds,ax
         
         mov bx,msg1
         call put_string                  ;��ʾ�ڶ�����Ϣ 

		 ; // ����ѭ��
         jmp $ 

;===============================================================================
SECTION code_2 align=16 vstart=0          ;��������2��16�ֽڶ��룩

  begin:
         push word [es:code_1_segment]
         mov ax,continue
         push ax                          ;����ֱ��push continue,80386+
         
         retf                             ;ת�Ƶ������1����ִ�� 
         
;===============================================================================
SECTION data_1 align=16 vstart=0
;// �ñ��� msg0 ����������һ����ַ�
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
           ; // �ӵ�ǰλ�ÿ�ʼ,���� 256 ���ֽڵ�ջ�ռ�,������ʼ�����ǵ�ֵ
         resb 256
		; // ջ�� stack vstart=0, �� stack_end �Ļ���ַ�� 256
stack_end:  

;===============================================================================
SECTION trail align=16
program_end: