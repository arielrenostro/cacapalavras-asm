C_COLUNAS EQU 60
C_LINHAS EQU 20
LENGTH_CACA_PALAVRAS EQU 1200
LENGTH_PALAVRA_DIGITADA EQU 25

COR_PRETO_BRANCO    	EQU 00001111B
COR_BRANCO_PRETO    	EQU 11110000B
COR_AZUL_PRETO      	EQU 00010000B    
COR_PRETO_CIANO     	EQU 00000011B 
COR_VERDE_PRETO     	EQU 00100000B
COR_CINZA_PRETO     	EQU 01110000B 
COR_CIANO_PRETO			EQU 00110000B
COR_DARK_PRETO 			EQU 10001000B  
COR_AZUL_BRANCO         EQU 00011111B

COR_PALAVRA_MARCADA 	EQU COR_CIANO_PRETO
COR_FUNDO           	EQU COR_CIANO_PRETO
COR_CACAPALAVRA			EQU COR_CINZA_PRETO
COR_BORDA_CACAPALAVRA 	EQU COR_DARK_PRETO
COR_CABECALHO 			EQU COR_BRANCO_PRETO
COR_BORDA_CABECALHO 	EQU COR_AZUL_PRETO


data segment
    ; CACAPALAVRA
    CACA_PALAVRAS 	        DB	"ASDFKASJFLKASJDFLKJASKRURIJDSLFJKLSAJKDSVMNMSDPFKVLSADFDSART"
							DB	"KSAOJFLKASJFLKASAJSDKFJSAEKFJKSADJFLKSAJFKLJSALKFJLKASJCFKAA"
							DB	"ASJDFKSADKFJSKAREJADSREWUUNJBKNBDKGFPTHHFKSJRKDAJDKSHHAASDBB"
							DB	"JDSAKFJESIURVMLNFSKTYRSIUOVNKLSDFGREUIFSLKGHROIGJFKSDNKLSUAA"
							DB	"RUQLIOEUROIQEWURIODSMSADFJKSAJVXCMJASDKFVSALKFJKLSADNFLKSDUU"
							DB	"ASKCFJKALSDJFLKASDJFLKSAJDFLKCJDKLFJSALTIFJLSDKAJKESASLKAHHC"
							DB	"RQWEIRUQWEISDFJHSADJFHSAUIFUIWSUERIUEWASDDJSATKLFJRDOKJFSSEE"
							DB	"ASDTFPOASIDFOPSAIDFOPAISDOPFIASOPFISAPODEIOPASDIFOPSAIFOOFHH"
							DB	"ASDFASDKFLSADKFLASKDFCLSAIDOFRIEJJVDFSAIOOSADIFOSDIOPFISDODD"
							DB	"ASPDFIASPOAFIPAOSDFIPACOMPUTADORFOPSADIFOPSDIFOPSDAOPFDSATAA"
							DB	"ASDFIEWOPRIEWOIREWOPQIROQWEIROPEWQIRIOWQEIOPRIWQORIWQEPOIREE"
							DB	"ASDFASDJRKASDJFKLASDJFLKDSAJFLKSDAJFAMLKCDJFLKCSADFDSKSLADEE"
							DB	"ASDFASDOISADFISOADIFASDFISAODIFOSADIFSPDPIFPOSADIFOPDSIISAAB"
							DB	"ASDOPFSADFINVNCXJDFSIFDSNFLSDAFUSADFUSDRIFUISADUFIODSAUISDDD"
							DB	"QWERUSIOWERUIOQWEURIOWEQUIRWEQUIORUQWOEIESNVDMSADFKLSJDALKBB"
							DB	"LSKDEPOWELNVMXNCFDSLKLSEYEFMDSHVAFDJNFSYFSSNFSDAYUEQSDSAAACC"
							DB	"ALSRKLASDKLCASDLIURIWEURIEWQURIOQEWURIOFDSSFJSADHFJKSDADDDDD"
							DB	"VXPMZXESUOMXZNCVMIEOIOPDSIFOSDIOFISDAOFIOSAOIFODSIFOSDAIODDD"
							DB	"QMIERUQIWERUIQWEURIOQWEURIQWUERIUQWEIRUQWIOERUIQWEURIEWUUQAA"
							DB	"IWERQUWIRUQWEIORUQIWOERUOIQWEUROIQWUEROIUQWOIAWRUIWSDVFSDHQQ"
    
    ; LAYOUT
	CABECALHO				DB "Ca",135,"a Palavras | FURB | Ariel Adonai Souza e Jardel Angelo dos Santos$"
	DIGITE_PALAVRA 			DB "Digite uma palavra: $"
	
    ; VARIAVEIS DE CONTROLE
    PALAVRA_DIGITADA        DB  "                         "
    IDX_PALAVRA_DIGITADA    DW -1
	IDX_CACA_PALAVRA 		DW 0
	ACHOU_PALAVRA 			DB 0
	TIPO_BUSCA 				DB 0
	ULTIMA_POSICAO_ANALISE  DW 0
ends

stack segment
    dw   128  dup(0)
ends

code segment
start:
    mov ax, data
    mov ds, ax
	mov ax, 0b800h ; SEGMENTO DE MEMORIA DO VIDEO
    mov es, ax
    
	
	
	
	
BEFORE_APLICACAO PROC
    call RESET
	jmp APLICACAO ; sim, n precisa desse jmp. Mas vai que coloquem algum codigo entre o before_aplicacao e a aplicacao...
BEFORE_APLICACAO ENDP








APLICACAO PROC
    call AGUARDAR_PALAVRA
    call PROCURAR_PALAVRA
    
    jmp APLICACAO  
APLICACAO ENDP








PROCURAR_PALAVRA PROC
	cmp ds:[ACHOU_PALAVRA], 1
	je MARCAR_PALAVRA
	
	cmp ds:[TIPO_BUSCA], 0 ; tipo_busca = 0, usado para saber se Ã© uma nova busca de palavra ou se Ã© uma chamada de um novo processo de busca
	jne PROCURAR_PALAVRA_CONTINUAR 
	
	call PROCURAR_LETRA_CACA_PALAVRA
	cmp dl, -1 ; dl = -1, significa que chegou ao final da string
	je PROCURAR_PALAVRA_FIM
	
	inc ds:[TIPO_BUSCA] ; indica o inicio da busca
	
	jmp PROCURAR_PALAVRA_CONTINUAR ; sim, n precisa mas Ã© a mesma historia do before_aplicacao

PROCURAR_PALAVRA_CONTINUAR:
	cmp ds:[TIPO_BUSCA], 1
	je PROCURAR_PALAVRA_ESQUERDA
	
	cmp ds:[TIPO_BUSCA], 2
	je PROCURAR_PALAVRA_DIREITA
	
	cmp ds:[TIPO_BUSCA], 3
	je PROCURAR_PALAVRA_ACIMA
	
	cmp ds:[TIPO_BUSCA], 4
	je PROCURAR_PALAVRA_ABAIXO
	
	cmp ds:[TIPO_BUSCA], 5
	je PROCURAR_PALAVRA_DIAG_ABAIXO_ESQUERDA
	
	cmp ds:[TIPO_BUSCA], 6
	je PROCURAR_PALAVRA_DIAG_ABAIXO_DIREITA
	
	cmp ds:[TIPO_BUSCA], 7
	je PROCURAR_PALAVRA_DIAG_ACIMA_ESQUERDA
	
	cmp ds:[TIPO_BUSCA], 8
	je PROCURAR_PALAVRA_DIAG_ACIMA_DIREITA
	
	inc ds:[IDX_CACA_PALAVRA] ; incrementa o idx_caca_palavra para seguir a partir da proxima letra
	
	cmp ds:[IDX_CACA_PALAVRA], LENGTH_CACA_PALAVRAS ; verifica se chegou ao final do caca_palavra
	jge PROCURAR_PALAVRA_FIM
	
	mov ds:[TIPO_BUSCA], 0 ; zera o tipo de busca para continuar e pesquisar a proxima letra no caca_palavra
	jmp PROCURAR_PALAVRA
	
PROCURAR_PALAVRA_FIM:
	call LIMPAR_PALAVRA_DIGITADA
	mov ds:[IDX_CACA_PALAVRA], 0
	mov ds:[TIPO_BUSCA], 0
	
	ret
PROCURAR_PALAVRA ENDP








PROCURAR_PALAVRA_BASE PROC ; di = idx_palavra_digitada; bx = idx_caca_palavra; ah = caractere palavra_digitada; cx = indice de incremento
	push bx
	call VALIDAR_POSICAO_CACA_PALAVRA
	
	cmp dl, -1 ; ERROU! - a posicao nao Ã© valida!
	je PROCURAR_PALAVRA_BASE_N_ACHOU
	
	mov ah, ds:[PALAVRA_DIGITADA][di]
	cmp ds:[CACA_PALAVRAS][bx], ah 
	je PROCURAR_PALAVRA_BASE_CONTINUAR ; se a letra do caÃ§a palavra sendo analisada for a mesma da palavra digitada, continua
	jmp PROCURAR_PALAVRA_BASE_N_ACHOU ; se a letra do caca palavra sendo analisada nÃ£o for a mesma da palavra digitada, sai!
	
PROCURAR_PALAVRA_BASE_CONTINUAR:
	cmp di, IDX_PALAVRA_DIGITADA ; compara se a posiÃ§Ã£o atual for maior que o tamanho da palavra_digitada
	jge PROCURAR_PALAVRA_BASE_MARCAR
	
	mov ds:[ULTIMA_POSICAO_ANALISE], bx
	
	inc di ; incrementa o indice da palavra digitada	
	add bx, cx ; incrementa o indice de incremento do caca palavra
	jmp PROCURAR_PALAVRA_BASE ; chama a funcao de novo para verificar o caractere
	
PROCURAR_PALAVRA_BASE_MARCAR:
	mov ds:[ACHOU_PALAVRA], 1
	jmp PROCURAR_PALAVRA_BASE_RET
	
PROCURAR_PALAVRA_BASE_N_ACHOU:
	inc ds:[TIPO_BUSCA] ; Ao final, muda o tipo de busca para mudar o tipo de busca
	jmp PROCURAR_PALAVRA_BASE_RET
	
PROCURAR_PALAVRA_BASE_RET:
	mov ds:[ULTIMA_POSICAO_ANALISE], 0 ; Ao final, zera a ultima posicao de analise
	
	ret
PROCURAR_PALAVRA_BASE ENDP








PROCURAR_PALAVRA_ESQUERDA PROC
	push bx
	push cx
	push di
	push dx
	
	mov cx, -1
	mov di, 0
	mov bx, ds:[IDX_CACA_PALAVRA]
	call PROCURAR_PALAVRA_BASE
	
	pop dx
	pop di
	pop cx
	pop bx
	
	jmp PROCURAR_PALAVRA
PROCURAR_PALAVRA_ESQUERDA ENDP








PROCURAR_PALAVRA_DIREITA PROC
	push bx
	push cx
	push di
	push dx
	
	mov cx, 1
	mov di, 0
	mov bx, ds:[IDX_CACA_PALAVRA]
	call PROCURAR_PALAVRA_BASE
	
	pop dx
	pop di
	pop cx
	pop bx
	
	jmp PROCURAR_PALAVRA
PROCURAR_PALAVRA_DIREITA ENDP








PROCURAR_PALAVRA_ACIMA PROC
	push bx
	push cx
	push di
	push dx
	
	mov cx, - C_COLUNAS
	mov di, 0
	mov bx, ds:[IDX_CACA_PALAVRA]
	call PROCURAR_PALAVRA_BASE
	
	pop dx
	pop di
	pop cx
	pop bx
	
	jmp PROCURAR_PALAVRA
PROCURAR_PALAVRA_ACIMA ENDP








PROCURAR_PALAVRA_ABAIXO PROC
	push bx
	push cx
	push di
	push dx
	
	mov cx, C_COLUNAS
	mov di, 0
	mov bx, ds:[IDX_CACA_PALAVRA]
	call PROCURAR_PALAVRA_BASE
	
	pop dx
	pop di
	pop cx
	pop bx
	
	jmp PROCURAR_PALAVRA
PROCURAR_PALAVRA_ABAIXO ENDP








PROCURAR_PALAVRA_DIAG_ABAIXO_ESQUERDA PROC
	push bx
	push cx
	push di
	push dx
	
	mov cx, C_COLUNAS - 1
	mov di, 0
	mov bx, ds:[IDX_CACA_PALAVRA]
	call PROCURAR_PALAVRA_BASE
	
	pop dx
	pop di
	pop cx
	pop bx
	
	jmp PROCURAR_PALAVRA
PROCURAR_PALAVRA_DIAG_ABAIXO_ESQUERDA ENDP








PROCURAR_PALAVRA_DIAG_ABAIXO_DIREITA PROC
	push bx
	push cx
	push di
	push dx
	
	mov cx, C_COLUNAS + 1
	mov di, 0
	mov bx, ds:[IDX_CACA_PALAVRA]
	call PROCURAR_PALAVRA_BASE
	
	pop dx
	pop di
	pop cx
	pop bx
	
	jmp PROCURAR_PALAVRA
PROCURAR_PALAVRA_DIAG_ABAIXO_DIREITA ENDP








PROCURAR_PALAVRA_DIAG_ACIMA_ESQUERDA PROC
	push bx
	push cx
	push di
	push dx
	
	mov cx, - C_COLUNAS - 1
	mov di, 0
	mov bx, ds:[IDX_CACA_PALAVRA]
	call PROCURAR_PALAVRA_BASE
	
	pop dx
	pop di
	pop cx
	pop bx
	
	jmp PROCURAR_PALAVRA
PROCURAR_PALAVRA_DIAG_ACIMA_ESQUERDA ENDP








PROCURAR_PALAVRA_DIAG_ACIMA_DIREITA PROC
	push bx
	push cx
	push di
	push dx
	
	mov cx, - C_COLUNAS + 1
	mov di, 0
	mov bx, ds:[IDX_CACA_PALAVRA]
	call PROCURAR_PALAVRA_BASE
	
	pop dx
	pop di
	pop cx
	pop bx
	
	jmp PROCURAR_PALAVRA
PROCURAR_PALAVRA_DIAG_ACIMA_DIREITA ENDP








VALIDAR_POSICAO_CACA_PALAVRA PROC ; Valida a posicao atual com a anterior; DL=0 = TRUE !; DL=-1 = FALSE !; deve ser passado um indice de incremento na stack
	push ax
	push bx
	push cx
	push bp
	
	mov bp, sp
	
	mov ax, ds:[ULTIMA_POSICAO_ANALISE] 
	cmp ax, 0 ; Ã© a primeira analise do caca palavra
	je VALIDAR_POSICAO_CACA_PALAVRA_TRUE
	
	mov dx, ss:[bp+10] ; pega o indice
	
	cmp dx, 0 ; Posicao invalida.
	jl VALIDAR_POSICAO_CACA_PALAVRA_FALSE
	
	cmp dx, LENGTH_CACA_PALAVRAS ;posicao invalida
	jg VALIDAR_POSICAO_CACA_PALAVRA_FALSE
	
	mov bx, C_COLUNAS 
	div bl
	
	mov cx, ax ; guarda a divisao em cx
	
	mov ax, dx ; passa o indice para ser dividido
	mov bx, C_COLUNAS
	div bl
	
	cmp ds:[TIPO_BUSCA], 1
	je VALIDAR_POSICAO_CACA_PALAVRA_HORIZONTAL
	
	cmp ds:[TIPO_BUSCA], 2
	je VALIDAR_POSICAO_CACA_PALAVRA_HORIZONTAL
	
	cmp ds:[TIPO_BUSCA], 3
	je VALIDAR_POSICAO_CACA_PALAVRA_VERTICAL
	
	cmp ds:[TIPO_BUSCA], 4
	je VALIDAR_POSICAO_CACA_PALAVRA_VERTICAL
	
	cmp ds:[TIPO_BUSCA], 5
	je VALIDAR_POSICAO_CACA_PALAVRA_DIAGONAL_ESQUERDA
	
	cmp ds:[TIPO_BUSCA], 6
	je VALIDAR_POSICAO_CACA_PALAVRA_DIAGONAL_DIREITA
	
	cmp ds:[TIPO_BUSCA], 7
	je VALIDAR_POSICAO_CACA_PALAVRA_DIAGONAL_ESQUERDA
	
	cmp ds:[TIPO_BUSCA], 8
	je VALIDAR_POSICAO_CACA_PALAVRA_DIAGONAL_DIREITA
	
VALIDAR_POSICAO_CACA_PALAVRA_HORIZONTAL: ; divide a ultima posicao e a posiÃ§Ã£o atual por 60, e compara se estao na mesma linha
	cmp cl, al ; compara se a ultima posicao e a posicao atual estÃ£o na mesma linha
	je VALIDAR_POSICAO_CACA_PALAVRA_TRUE
	jmp VALIDAR_POSICAO_CACA_PALAVRA_FALSE
	
VALIDAR_POSICAO_CACA_PALAVRA_VERTICAL: ; aplica o modulo de 60 na ultima posicao e na posicao atual. Compara se estao na mesma coluna
	cmp ch, ah ; compara se a ultima posicao e a posicao atual estao na mesma coluna
	je VALIDAR_POSICAO_CACA_PALAVRA_TRUE
	jmp VALIDAR_POSICAO_CACA_PALAVRA_FALSE
	
VALIDAR_POSICAO_CACA_PALAVRA_DIAGONAL_ESQUERDA: ; aplica o modulo de 60 na ultima posicao e na posicao atual. Compara se a anterior Ã© maior que a atual.
	cmp ch, ah ; compara se a posicao anterior Ã© maior que a atual
	jg VALIDAR_POSICAO_CACA_PALAVRA_TRUE
	jmp VALIDAR_POSICAO_CACA_PALAVRA_FALSE
	
VALIDAR_POSICAO_CACA_PALAVRA_DIAGONAL_DIREITA: ; aplica o modulo de 60 na ultima posicao e na posicao atual. Compara se a anterior Ã© menor que a atual.
	cmp ch, ah ; compara se a posicao anterior Ã© menor que a atual
	jl VALIDAR_POSICAO_CACA_PALAVRA_TRUE
	jmp VALIDAR_POSICAO_CACA_PALAVRA_FALSE
	
VALIDAR_POSICAO_CACA_PALAVRA_FALSE:
	mov dl, -1
	jmp VALIDAR_POSICAO_CACA_PALAVRA_FIM
	
VALIDAR_POSICAO_CACA_PALAVRA_TRUE:
	mov dl, 0
	jmp VALIDAR_POSICAO_CACA_PALAVRA_FIM
	
VALIDAR_POSICAO_CACA_PALAVRA_FIM:
	pop bp
	pop cx
	pop bx
	pop ax
	
	ret 2
VALIDAR_POSICAO_CACA_PALAVRA ENDP








MARCAR_PALAVRA PROC
	push ax
	push bx
	push cx
	push dx
	
	mov dx, 0
	mov ax, ds:[IDX_CACA_PALAVRA]
	mov bl, C_COLUNAS
	div bl ; al = resultado; ah = modulo; resultado Ã© a linha que a palavra estÃ¡
	mov dl, ah ; salva o mÃ³dulo (coluna) em dl
	
	mov bl, 160 
	mul bl ; multiplica a linha atual pela quantidade de posicoes do monitor por linha; ax = linha da palavra
	add ax, 320 ; adiciona as linhas de cabecalho

	rol dl, 1 ; multiplica a coluna por 2.
	add dx, 18 + 1; adiciona a margem + impar
	
	mov bx, dx ; adiciona a coluna da palavra
	add bx, ax ; adiciona a linha da palavra
	
	mov cx, ds:[IDX_PALAVRA_DIGITADA] ; passa para cx o tamanho da palavra_digitada
	inc cx ; corrige
	
	cmp ds:[TIPO_BUSCA], 1
	je MARCAR_PALAVRA_ESQUERDA
	
	cmp ds:[TIPO_BUSCA], 2
	je MARCAR_PALAVRA_DIREITA
	
	cmp ds:[TIPO_BUSCA], 3
	je MARCAR_PALAVRA_ACIMA
	
	cmp ds:[TIPO_BUSCA], 4
	je MARCAR_PALAVRA_ABAIXO
	
	cmp ds:[TIPO_BUSCA], 5
	je MARCAR_PALAVRA_DIAG_ABAIXO_ESQUERDA
	
	cmp ds:[TIPO_BUSCA], 6
	je MARCAR_PALAVRA_DIAG_ABAIXO_DIREITA
	
	cmp ds:[TIPO_BUSCA], 7
	je MARCAR_PALAVRA_DIAG_ACIMA_ESQUERDA
	
	cmp ds:[TIPO_BUSCA], 8
	je MARCAR_PALAVRA_DIAG_ACIMA_DIREITA
	
	jmp MARCAR_PALAVRA_RET
	
MARCAR_PALAVRA_ESQUERDA:
	mov dx, -2
	jmp MARCAR_PALAVRA_BASE

MARCAR_PALAVRA_DIREITA:
	mov dx, 2
	jmp MARCAR_PALAVRA_BASE

MARCAR_PALAVRA_ACIMA:
	mov dx, -160
	jmp MARCAR_PALAVRA_BASE

MARCAR_PALAVRA_ABAIXO:
	mov dx, 160
	jmp MARCAR_PALAVRA_BASE

MARCAR_PALAVRA_DIAG_ABAIXO_ESQUERDA:
	mov dx, 158
	jmp MARCAR_PALAVRA_BASE

MARCAR_PALAVRA_DIAG_ABAIXO_DIREITA:
	mov dx, 162
	jmp MARCAR_PALAVRA_BASE

MARCAR_PALAVRA_DIAG_ACIMA_ESQUERDA:
	mov dx, -162
	jmp MARCAR_PALAVRA_BASE

MARCAR_PALAVRA_DIAG_ACIMA_DIREITA:
	mov dx, -158
	jmp MARCAR_PALAVRA_BASE
	
MARCAR_PALAVRA_BASE:
	mov es:[bx], COR_PALAVRA_MARCADA
	add bx, dx
	loop MARCAR_PALAVRA_BASE
	
	jmp MARCAR_PALAVRA_RET
	
MARCAR_PALAVRA_RET:	
	mov ds:[ACHOU_PALAVRA], 0
	
	pop dx
	pop cx
	pop bx
	pop ax
	
	jmp PROCURAR_PALAVRA_FIM
MARCAR_PALAVRA ENDP








LIMPAR_PALAVRA_DIGITADA PROC
	push bx
	push cx
	
	mov ds:[IDX_PALAVRA_DIGITADA], -1
	
	mov cx, 25
LIMPAR_PALAVRA_DIGITADA_LOOP:
	mov bx, cx ; pega o valor do loop para usar como indice
	dec bx ; corrige o indice
	mov PALAVRA_DIGITADA[bx], " "
	loop LIMPAR_PALAVRA_DIGITADA_LOOP
	
	pop cx
	pop bx
	
	ret
LIMPAR_PALAVRA_DIGITADA ENDP








PROCURAR_LETRA_CACA_PALAVRA PROC ;dl = 0 achou; dl = -1 n achou
	push ax
	push bx
	push cx

	mov dl, -1 ; limpa o dl
	mov ah, 0
	mov al, ds:[PALAVRA_DIGITADA] ; pega o primeiro caractere digitado para pesquisar no caÃ§a palavra
	
	mov bx, ds:[IDX_CACA_PALAVRA]
	
	mov cx, LENGTH_CACA_PALAVRAS ; tamanho total do caca palavra
	sub cx, bx ; subtrai a posiÃ§Ã£o que ele estÃ¡
	
PROCURAR_LETRA_CACA_PALAVRA_LOOP:
	cmp ds:[CACA_PALAVRAS][bx], al ; verifica se a posicao da string 
	je PROCURAR_LETRA_CACA_PALAVRA_ACHOU
	
	inc bx
	loop PROCURAR_LETRA_CACA_PALAVRA_LOOP
	
	jmp PROCURAR_LETRA_CACA_PALAVRA_RET

PROCURAR_LETRA_CACA_PALAVRA_ACHOU:
	mov dl, 0
	jmp PROCURAR_LETRA_CACA_PALAVRA_RET ; sim, n precisa mas Ã© a mesma historia do before_aplicacao
	
PROCURAR_LETRA_CACA_PALAVRA_RET:
	mov ds:[IDX_CACA_PALAVRA], bx
	pop cx
	pop bx
	pop ax
	
	ret
PROCURAR_LETRA_CACA_PALAVRA ENDP








AGUARDAR_PALAVRA PROC 
    push ax
    push bx
    push cx   
    
AGUARDAR_PALAVRA_POS:
    call PRINT_PALAVRA_DIGITADA
    call LER_TECLA
	
	cmp al, 27
	je HALT

	cmp al, 9 ; resetar
	je BEFORE_APLICACAO
	
    cmp al, 8
    je AGUARDAR_PALAVRA_BACKSPACE
    
    cmp al, 13
    je AGUARDAR_PALAVRA_RET
	
	cmp al, 65
    jl AGUARDAR_PALAVRA_POS
    
    cmp al, 90
    jg AGUARDAR_PALAVRA_POS
    
    mov cx, ds:[IDX_PALAVRA_DIGITADA]
    cmp cx, 24
    je AGUARDAR_PALAVRA_POS
    
    inc cx
    lea bx, PALAVRA_DIGITADA
    add bx, cx
    mov ds:[bx], al
    inc ds:[IDX_PALAVRA_DIGITADA]
  
    jmp AGUARDAR_PALAVRA_POS
    
AGUARDAR_PALAVRA_BACKSPACE:
    mov cx, ds:[IDX_PALAVRA_DIGITADA]
    
    cmp cx, -1
    je AGUARDAR_PALAVRA_POS
    
    lea bx, PALAVRA_DIGITADA
    add bx, cx
    mov ds:[bx], " "
    dec ds:[IDX_PALAVRA_DIGITADA]
    jmp AGUARDAR_PALAVRA_POS
    
AGUARDAR_PALAVRA_RET:
    pop cx
    pop bx
    pop ax
	
    ret
AGUARDAR_PALAVRA ENDP








RESET PROC
	mov ah, 0
	mov al, 03h
	int 10h ;RESET MODO VIDEO

    mov ax, 0
    mov bx, 0
    mov cx, 0
    mov dx, 0 
    
    mov ds:[IDX_PALAVRA_DIGITADA], -1
	mov ds:[IDX_CACA_PALAVRA], 0
	mov ds:[ACHOU_PALAVRA], 0
    
    call CLEAR_SCREEN
	call PRINT_LAYOUT
    
    ret
RESET ENDP   








PRINT_PALAVRA_DIGITADA PROC
    push bx ; idx_palavra_digitada
    push cx ; loop
    push dx ; posicao_cursor e print palavra
    
    mov bx, 24 ; idx_palavra_digitada
    mov cx, 25 ; qnt de loops
	mov dh, 23 ; linha para printar palavra_digitada
	mov dl, 47 ; coluna para printar palavra_digitada
	jmp PRINT_PALAVRA_DIGITADA_LOOP
	
PRINT_PALAVRA_DIGITADA_LOOP:
	dec dl ; subtrai um para ajustar a posicao do cursor
    call SET_CURSOR
    
	push dx
	mov dl, ds:[PALAVRA_DIGITADA][bx] ; pega a letra da string
    call PRINT_CHAR
	pop dx
	
	dec bl ; subtrai o indice
    loop PRINT_PALAVRA_DIGITADA_LOOP

	mov bx, ds:[IDX_PALAVRA_DIGITADA] ; pega o indice da palavra_digitada
	add bx, 23 ; adiciona a margem da tela
	mov dl, bl
	call SET_CURSOR
    
    pop dx 
    pop cx
    pop bx
	
    ret 
PRINT_PALAVRA_DIGITADA ENDP









PRINT_CACA_PALAVRA PROC               
    push ax
    push bx
    push cx
    push dx   
    
	mov dh, 2 ;linha
	mov dl, 9 ;coluna
	call SET_CURSOR
	
    mov al, 1 ; linha
	mov bx, 0 ; idx_caca_palavra
    
PRINT_CACA_PALAVRA_NOVA_LINHA:
    mov cx, C_COLUNAS 
    inc al

PRINT_CACA_PALAVRA_LOOP:
    mov dl, CACA_PALAVRAS[bx]
    call PRINT_CHAR  
    inc bx
    loop PRINT_CACA_PALAVRA_LOOP
    
    mov dh, al
	add dh, 1
	mov dl, 9
	call SET_CURSOR
    
    cmp al, C_LINHAS 
    jle PRINT_CACA_PALAVRA_NOVA_LINHA
	
	mov bx, 339
	mov cl, 60
	mov ch, 20
PRINT_CACA_PALAVRA_FUNDO:
	mov es:[bx], COR_CACAPALAVRA
	add bx, 2
	dec cl
	cmp cl, 0
	jg PRINT_CACA_PALAVRA_FUNDO
    
	dec ch
	add bx, 40
	mov cl, 60
	cmp ch, 0
	jg PRINT_CACA_PALAVRA_FUNDO
	
    pop dx
    pop cx 
    pop bx 
    pop ax
	
    ret
PRINT_CACA_PALAVRA ENDP








PRINT_MSG PROC
    push ax
    
    mov ah, 9
    int 21h
    
    pop ax
	
    ret       
PRINT_MSG ENDP 








PRINT_CHAR PROC
    push ax
    
    mov ah, 2
    int 21H
    
    pop ax
	
    ret
PRINT_CHAR ENDP








PRINT_COR_FUNDO PROC
	push bx
	push cx

	mov cx, 4000
	
PRINT_COR_FUNDO_LOOP:
	dec cx
	mov bx, cx
	mov es:[bx], COR_FUNDO
	loop PRINT_COR_FUNDO_LOOP
	
	pop cx
	pop bx
	
	ret
PRINT_COR_FUNDO ENDP








PRINT_CABECALHO PROC
	push bx
	push dx
	
	mov dh, 0 ;linha
	mov dl, 5 ;coluna
	call SET_CURSOR
	
	lea dx, CABECALHO
	call PRINT_MSG
	
	mov bx, 11
PRINT_CABECALHO_LOOP:
	mov es:[bx], COR_CABECALHO
	add bx, 2
	cmp bx, 147
	jl PRINT_CABECALHO_LOOP
	
	pop dx
	pop bx
	
	ret
PRINT_CABECALHO ENDP








PRINT_MSG_DIGITE_PALAVRA PROC
    push dx ; posicao_cursor e print palavra
    
	mov dh, 23 ; linha para printar palavra_digitada
	mov dl, 2 ; coluna para printar palavra_digitada
	call SET_CURSOR
    
	lea dx, ds:[DIGITE_PALAVRA]
	call PRINT_MSG
	
	pop dx

	ret
PRINT_MSG_DIGITE_PALAVRA ENDP








PRINT_BORDAS PROC
	push cx
	push dx
	
	; BORDA CABECALHO
	;mov es:[9], COR_BORDA_CABECALHO
	;mov es:[147], COR_BORDA_CABECALHO
	;mov cx, 140
	
PRINT_BORDAS_CABECALHO_LOOP:
    ;mov bx, cx
    ;add bx, 167
    ;mov es:[bx], COR_BORDA_CABECALHO
	;dec cx
	;loop PRINT_BORDAS_CABECALHO_LOOP
	
	; BORDA CACA_PALAVRAS
	mov bx, 177
PRINT_BORDA1_CACA_PALAVRAS_LOOP:
	mov es:[bx], COR_BORDA_CACAPALAVRA
	add bx, 2
	cmp bx, 177 + 124
	jl PRINT_BORDA1_CACA_PALAVRAS_LOOP
	
	mov bx, 3537
PRINT_BORDA2_CACA_PALAVRAS_LOOP:
	mov es:[bx], COR_BORDA_CACAPALAVRA
	add bx, 2
	cmp bx, 3537 + 124
	jl PRINT_BORDA2_CACA_PALAVRAS_LOOP

	mov bx, 177
PRINT_BORDA3_CACA_PALAVRAS_LOOP:
	mov es:[bx], COR_BORDA_CACAPALAVRA
	add bx, 160
	cmp bx, 3537
	jl PRINT_BORDA3_CACA_PALAVRAS_LOOP
	
	mov bx, 177 + 122
PRINT_BORDA4_CACA_PALAVRAS_LOOP:
	mov es:[bx], COR_BORDA_CACAPALAVRA
	add bx, 160
	cmp bx, 3537 + 122
	jl PRINT_BORDA4_CACA_PALAVRAS_LOOP
	
	pop dx
	pop cx
	
	ret
PRINT_BORDAS ENDP








PRINT_LAYOUT PROC
	call PRINT_COR_FUNDO
	call PRINT_CABECALHO
	call PRINT_CACA_PALAVRA
	call PRINT_MSG_DIGITE_PALAVRA
	call PRINT_BORDAS
	
	ret
PRINT_LAYOUT ENDP








LER_TECLA PROC
    mov ah, 8
    int 21H
	
    ret    
LER_TECLA ENDP 








SET_CURSOR PROC ; BH = Page Number, DH = Row, DL = Column
    push ax
    push bx
    
    mov bx, 0
    mov ah, 2h
    int 10h   
    
    pop bx
    pop ax
	
    ret
SET_CURSOR ENDP








CLEAR_SCREEN PROC NEAR
	push    ax      ; store registers...
	push    ds      ;
	push    bx      ;
	push    cx      ;
	push    di      ;

	mov     ax, 40h
	mov     ds, ax  ; for getting screen parameters.
	mov     ah, 06h ; scroll up function id.
	mov     al, 0   ; scroll all lines!
	mov     bh, 07  ; attribute for new lines.
	mov     ch, 0   ; upper row.
	mov     cl, 0   ; upper col.
	mov     di, 84h ; rows on screen -1,
	mov     dh, [di] ; lower row (byte).
	mov     di, 4ah ; columns on screen,
	mov     dl, [di]
	dec     dl      ; lower col.
	int     10h

	; set cursor position to top
	; of the screen:
	mov     bh, 0   ; current page.
	mov     dl, 0   ; col.
	mov     dh, 0   ; row.
	mov     ah, 02
	int     10h

	pop     di      ; re-store registers...
	pop     cx      ;
	pop     bx      ;
	pop     ds      ;
	pop     ax      ;

	ret
CLEAR_SCREEN ENDP  








FREEZE PROC
    jmp FREEZE
FREEZE ENDP








AGUARDAR_TECLA PROC
    push ax
	
	call LER_TECLA
	
	pop ax
	
	ret
AGUARDAR_TECLA ENDP








HALT PROC
    mov ax, 4C00H
    int 21H   
HALT ENDP

ends

end start
