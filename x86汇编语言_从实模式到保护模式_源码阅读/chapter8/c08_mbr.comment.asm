         ;代码清单8-1
         ;文件名：c08_mbr.asm
         ;文件说明：硬盘主引导扇区代码（加载程序） 
         ;创建日期：2011-5-5 18:17
         
		 ; // 编译 c08.asm 所产生的二进制文件,是写入虚拟硬盘的逻辑 100 扇区
         app_lba_start equ 100           ;声明常数（用户程序起始逻辑扇区号）
                                         ;常数的声明不会占用汇编地址
; // 硬盘主引导扇区地址是 0x0000:0x7c00, 物理地址 0x07c00
; // 定义 mbr 段的时候使用了 vstart=0x7c00, 那么段内所有汇编地址都是在 0x7c00 的基础上增加的,就不需要再加上 0x7c00
SECTION mbr align=16 vstart=0x7c00      ; 定义 mbr 段, 在内存中的汇编地址是16字节对齐,是16的倍数,或者说物理地址能被 16 整除
										; vstart=0x7c00 表示引用 mbr 段内的标号时,标号处的汇编地址是从 0x7c00 开始计算

         ;设置堆栈段和栈指针 
         mov ax,0      
         mov ss,ax
         mov sp,ax
      
		; // phy_base 是 32位变量,表示加载用户程序需要确定的物理地址
		; // 根据 x86 cpu 的低端序列特性, 32位变量需要用两个16位寄存器存放, 高 16 位存放在 phy_base+0x02 处, 低 16 位存放在 phy_base 处
		; // 使用了段超越前缀 cs:, 表示访问内存单元是,使用 cs 的内容( 0x0000 ) 作为段基址
         mov ax,[cs:phy_base]            ;计算用于加载用户程序的逻辑段地址 
         mov dx,[cs:phy_base+0x02]
		
		; // 32位变量 phy_base 表示用户程序物理地址, 除以 16 , 商作为段地址保存在 ax 中, 本例中是 0x1000
         mov bx,16        
         div bx            
         mov ds,ax                       ;令DS和ES指向该段以进行操作
         mov es,ax                        
    
         ;以下读取程序的起始部分 
		 ; // 目的变址寄存器 di 清零. 一般情况下 di 与ds联用,来确定某个储存单元的地址.
         xor di,di
		 ; // 把 用户程序起始逻辑扇区号 保存到 si 中; si 是源变址寄存器,默认段地址和 di 一样,在 ds 中.和 ds 联用. 
         mov si,app_lba_start            ;程序在硬盘上的起始逻辑扇区号 
		 ; // DI和SI这两个属于变址寄存器.可以和bx. bp联用,但是和 bx 连用时,段地址在 DS 中,和bp联用时,段地址在 SS 中.也可以单独使用,单独使用时,段地址默认在 DS 中,想要越段使用,加上段前缀即可.
         xor bx,bx                       ;加载到DS:0x0000处 
		 ; // 调用过程 read_hard_disk_0
         call read_hard_disk_0
      
         ;以下判断整个程序有多大
		 ; // 用户程序头部,加载到内存中后( DS 和 ES 指向此段内存 ),前面一个4字节的双字是用户程序总长度, 将这个双字数值的高16位和低16位分别送入 DX 和 AX
         mov dx,[2]                      ;曾经把dx写成了ds，花了二十分钟排错 
         mov ax,[0]
         mov bx,512                      ;512字节每扇区
		 ; // 用程序头部长度除以 512, AX 中是商表示扇区数, DX 中是余数表示不足一个扇区的字节数
         div bx
         cmp dx,0
         jnz @1                          ;未除尽，因此结果比实际扇区数少1 
         dec ax                          ;已经读了一个扇区，扇区总数减1 
   @1:
         cmp ax,0                        ;考虑实际长度小于等于512个字节的情况 
         jz direct
         
         ;读取剩余的扇区
         push ds                         ;以下要用到并改变DS寄存器 

         mov cx,ax                       ;循环次数（剩余扇区数）
   @2:
         mov ax,ds
         add ax,0x20                     ;得到下一个以512字节为边界的段地址
         mov ds,ax  
                              
         xor bx,bx                       ;每次读时，偏移地址始终为0x0000 
         inc si                          ;下一个逻辑扇区 
         call read_hard_disk_0
         loop @2                         ;循环读，直到读完整个功能程序 

         pop ds                          ;恢复数据段基址到用户程序头部段 
      
         ;计算入口点代码段基址 
   direct:
   ; // 程序头部 0x06 开始的4个字节是程序入口点的汇编地址,把这个32位的数保存在 DX:AX 中
         mov dx,[0x08]
         mov ax,[0x06]
		 ; // 计算出入口点代码段的汇编地址,存放在 ax 中
         call calc_segment_base
         mov [0x06],ax                   ;回填修正后的入口点代码段基址 
      
         ;开始处理段重定位表
		 ; // 程序头部 0x0a 的两个字节保存了段重定位表项数量
         mov cx,[0x0a]                   ;需要重定位的项目数量
		 ; // 程序头部 0x0c 的两个字节保存了段重定位表首地址
         mov bx,0x0c                     ;重定位表首地址
          
		  ; // 计算出每个段的地址,并回填到程序头部中对应的重定向表首地址处
 realloc:
         mov dx,[bx+0x02]                ;32位地址的高16位 
         mov ax,[bx]
         call calc_segment_base
         mov [bx],ax                     ;回填段的基址
         add bx,4                        ;下一个重定位项（每项占4个字节） 
         loop realloc 
      
	  ; // 用户程序头部 0x04 保存了程序入口点的偏移地址, 0x06-0xa 保存了段地址
	  ; // jmp 会访问段寄存器 DS 所指向的数据段,并从偏移地址 0x04 的地方取两个字传送给代码段寄存器 cs 和指令指针寄存器 ip
	  ; // 这样处理器就会自行转移到指定的位置开始执行
         jmp far [0x04]                  ;转移到用户程序  
 
;-------------------------------------------------------------------------------
read_hard_disk_0:                        ;从硬盘读取一个逻辑扇区
                                         ;输入：DI:SI=起始逻辑扇区号
                                         ;      DS:BX=目标缓冲区地址
         push ax
         push bx
         push cx
         push dx
      
         mov dx,0x1f2
         mov al,1
         out dx,al                       ;读取的扇区数

         inc dx                          ;0x1f3
         mov ax,si
         out dx,al                       ;LBA地址7~0

         inc dx                          ;0x1f4
         mov al,ah
         out dx,al                       ;LBA地址15~8

         inc dx                          ;0x1f5
         mov ax,di
         out dx,al                       ;LBA地址23~16

         inc dx                          ;0x1f6
         mov al,0xe0                     ;LBA28模式，主盘
         or al,ah                        ;LBA地址27~24
         out dx,al

         inc dx                          ;0x1f7
         mov al,0x20                     ;读命令
         out dx,al

  .waits:
         in al,dx
         and al,0x88
         cmp al,0x08
         jnz .waits                      ;不忙，且硬盘已准备好数据传输 

         mov cx,256                      ;总共要读取的字数
         mov dx,0x1f0
  .readw:
         in ax,dx
         mov [bx],ax
         add bx,2
         loop .readw

         pop dx
         pop cx
         pop bx
         pop ax
      
         ret

;-------------------------------------------------------------------------------
calc_segment_base:                       ;计算16位段地址
                                         ;输入：DX:AX=32位物理地址
                                         ;返回：AX=16位段基地址 
         push dx                          
         
         add ax,[cs:phy_base]
         adc dx,[cs:phy_base+0x02]
         shr ax,4
         ror dx,4
         and dx,0xf000
         or ax,dx
         
         pop dx
         
         ret

;-------------------------------------------------------------------------------
         phy_base dd 0x10000             ;用户程序被加载的物理起始地址
         
 times 510-($-$$) db 0
                  db 0x55,0xaa