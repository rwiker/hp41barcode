var one_byte_builtins = {
  'X!=Y?': 121,
  'MEAN': 124,
  'X=0?': 103,
  'GRAD': 130,
  'X<=Y?': 70,
  'X>0?': 100,
  'PSE': 137,
  'RDN': 117,
  'INT': 104,
  'X<Y?': 68,
  'R^': 116,
  'DEG': 128,
  'PI': 114,
  'ABS': 97,
  'X=Y?': 120,
  'LN1+X': 101,
  'CLX': 119,
  'RND': 110,
  'X>Y?': 69,
  'RTN': 133,
  'ASIN': 92,
  '%': 76,
  'X^2': 81,
  '10^X': 87,
  'AON': 140,
  'ENTER': 131,
  'AVIEW': 126,
  '*': 66,
  '+': 64,
  'ACOS': 93,
  '-': 65,
  '/': 67,
  '~+': 71,
  'FRC': 105,
  'PROMPT': 142,
  'TAN': 91,
  '}HMS': 108,
  'AOFF': 139,
  'CL+': 112,
  'SQRT': 82,
  'SDEV': 125,
  '~-': 72,
  'E^X': 85,
  'P->R': 78,
  'D->R': 106,
  'ADV': 143,
  '1/X': 96,
  'FACT': 98,
  '->DEC': 95,
  'Y^X': 83,
  'COS': 90,
  '->HR': 109,
  'CHS': 84,
  '}OCT': 111,
  'ENTER^': 131,
  'X<>Y': 113,
  '->HMS': 108,
  'NULL': 0,
  '->OCT': 111,
  'OFF': 141,
  'MOD': 75,
  'LASTX': 118,
  'ASHF': 136,
  'HMS-': 74,
  'R}P': 79,
  'X NE Y?': 121,
  'E^X-1': 88,
  'HMS+': 73,
  'CLST': 115,
  'ATAN': 94,
  'CLA': 135,
  'RAD': 129,
  'D}R': 106,
  'R->P': 79,
  'CLRG': 138,
  '}DEC': 95,
  'P}R': 78,
  '%CH': 77,
  'R->D': 107,
  '}HR': 109,
  'SIGN': 122,
  'X!=0?': 99,
  'BEEP': 134,
  'LOG': 86,
  'RUP': 116,
  'X<=0?': 123,
  'STOP': 132,
  'CLD': 127,
  'SIN': 89,
  'LN': 80,
  'X<0?': 102,
  'R}D': 107,
  'X NE 0?': 99
}; 
var two_byte_builtins = {
  'CF': 169,
  'RCL': 144,
  'FC?C': 171,
  'FIX': 156,
  'ARCL': 155,
  'ST-': 147,
  'ASTO': 154,
  'FS?': 172,
  'ISG': 150,
  '~REG': 153,
  'SF': 168,
  'SCI': 157,
  'X<>': 206,
  'ST*': 148,
  'STO': 145,
  'VIEW': 152,
  'STO2': 145,
  'FC?': 173,
  'TONE': 159,
  'ENG': 158,
  'FS?C': 170,
  'ST/': 149,
  'RCL2': 144,
  'DSE': 151,
  'ST+': 146
}; 
var xroms = {};
xroms['wand'] = {
  'WNDSCN': [27, 5],
  'WNDDTA': [27, 1],
  'WNDDTX': [27, 2],
  'WNDTST': [27, 6],
  'WNDSUB': [27, 4],
  'WNDLNK': [27, 3]
}; 
xroms['time'] = {
  'SETDATE': [26, 22],
  'ATIME24': [26, 5],
  'ADATE': [26, 1],
  'TIME': [26, 27],
  'CLKTD': [26, 9],
  'T+X': [26, 26],
  'SW': [26, 25],
  'CLK12': [26, 6],
  'MDY': [26, 17],
  'SETAF': [26, 21],
  'DMY': [26, 15],
  'CLK24': [26, 7],
  'RCLSW': [26, 19],
  'STOPSW': [26, 24],
  'DATE+': [26, 13],
  'CLOCK': [26, 10],
  'DOW': [26, 16],
  'XYZALM': [26, 28],
  'ALMNOW': [26, 3],
  'SETSW': [26, 23],
  'ATIME': [26, 4],
  'CLKT': [26, 8],
  'RCLAF': [26, 18],
  'DATE': [26, 12],
  'RUNSW': [26, 20],
  'DDAYS': [26, 14],
  'CORRECT': [26, 11],
  'ALMCAT': [26, 2]
}; 
xroms['printer'] = {
  'PRPLOT': [29, 14],
  'PRREGX': [29, 17],
  'ACA': [29, 1],
  'PR~': [29, 18],
  'PRBUF': [29, 10],
  'REGPLOT': [29, 21],
  'SKPCOL': [29, 23],
  'ACSPEC': [29, 4],
  'PRSTK': [29, 19],
  'PRPLOTP': [29, 15],
  'PRA': [29, 8],
  'PRAXIS': [29, 9],
  'LIST': [29, 7],
  'PRKEYS': [29, 12],
  'ACCOL': [29, 3],
  'ACCHR': [29, 2],
  'BLDSPEC': [29, 6],
  'PRREG': [29, 16],
  'PRFLAGS': [29, 11],
  'ACX': [29, 5],
  'STKPLOT': [29, 24],
  'PRX': [29, 20],
  'SKPCHR': [29, 22],
  'PRP': [29, 13]
}; 
xroms['ppc'] = {
  '~C': [10, 21],
  'M2': [20, 31],
  'SU': [10, 39],
  'RN': [20, 16],
  'TN': [10, 32],
  'UG': [20, 44],
  'S2': [20, 48],
  'AB': [10, 61],
  '+K': [10, 3],
  'BI': [10, 44],
  'CP': [20, 27],
  'DP': [10, 53],
  'CB': [10, 50],
  'EP': [10, 31],
  'FI': [10, 63],
  'HP': [20, 29],
  '1K': [10, 2],
  'IP': [10, 45],
  '2D': [10, 55],
  'MP': [20, 28],
  'LB': [10, 22],
  'NP': [20, 14],
  'L-': [10, 23],
  'M4': [20, 35],
  'RB': [20, 52],
  'SB': [20, 1],
  'TB': [20, 18],
  'BR': [20, 40],
  'AD': [10, 18],
  'CK': [10, 6],
  'DR': [20, 38],
  'BD': [20, 17],
  'CD': [10, 35],
  'FR': [20, 12],
  'FD': [20, 11],
  'IR': [20, 37],
  'HD': [10, 20],
  'LR': [20, 2],
  'NR': [20, 50],
  'MK': [10, 1],
  'PR': [20, 45],
  'QR': [10, 54],
  'PK': [10, 9],
  'PD': [10, 52],
  'SR': [20, 0],
  'RK': [20, 6],
  'SK': [20, 4],
  'RD': [20, 5],
  'SD': [20, 3],
  'VK': [10, 36],
  'UD': [10, 8],
  'AM': [20, 53],
  'XD': [10, 25],
  'BM': [20, 39],
  'A?': [10, 10],
  'CM': [20, 20],
  'DT': [10, 17],
  'DF': [20, 13],
  'C?': [10, 16],
  'E?': [10, 62],
  'F?': [10, 4],
  'IF': [10, 49],
  'MT': [10, 28],
  'LF': [10, 5],
  'OM': [10, 58],
  'PM': [20, 19],
  'RT': [10, 51],
  'M1': [20, 33],
  'SM': [20, 55],
  'RF': [10, 13],
  'S?': [10, 15],
  'VM': [10, 26],
  'VF': [20, 58],
  'S1': [20, 46],
  'T1': [10, 47],
  'BV': [20, 7],
  'CV': [20, 8],
  'BA': [20, 30],
  'CA': [20, 23],
  'HA': [20, 25],
  'MA': [20, 54],
  'NH': [10, 40],
  'PO': [20, 51],
  'M3': [20, 32],
  'SV': [20, 10],
  'PA': [10, 59],
  'S3': [20, 47],
  'VA': [10, 7],
  'BX': [20, 41],
  'CX': [10, 33],
  'BC': [20, 43],
  'CJ': [20, 21],
  'EX': [10, 27],
  'DC': [10, 11],
  'JC': [20, 22],
  'NC': [10, 38],
  'RX': [10, 57],
  'M5': [20, 36],
  'SX': [10, 56],
  '~?': [10, 14],
  'AL': [10, 37],
  'BL': [10, 42],
  'DS': [10, 29],
  'BE': [20, 34],
  'FL': [10, 43],
  'HS': [20, 26],
  'GE': [10, 60],
  'MS': [10, 48],
  'NS': [20, 49],
  'ML': [10, 12],
  'PS': [10, 46],
  'SE': [20, 56],
  'VS': [10, 30],
  'XL': [20, 57],
  'XE': [10, 19],
  'CU': [10, 34],
  '-B': [10, 24],
  'GN': [20, 15],
  'B+': [20, 42],
  'HN': [10, 41],
  'IG': [20, 9],
  'LG': [20, 24]
}; 
xroms['math'] = {
  'SSA': [1, 43],
  'CINV': [1, 21],
  'SIMEQ': [1, 2],
  'SOLVE': [1, 9],
  'C/': [1, 19],
  'Z^N': [1, 22],
  'C+': [1, 16],
  'Z^1/W': [1, 29],
  'SSS': [1, 39],
  'Z^W': [1, 28],
  'DET': [1, 6],
  'VCOL': [1, 3],
  'ACOSH': [1, 37],
  'SINH': [1, 33],
  'SOL': [1, 10],
  'SINZ': [1, 30],
  'MATRIX': [1, 1],
  'FOUR': [1, 15],
  'TRANS': [1, 44],
  'Z^1/N': [1, 23],
  'ATANH': [1, 38],
  'PVT': [1, 5],
  'COSH': [1, 34],
  'COSZ': [1, 31],
  'ASINH': [1, 36],
  'INTG': [1, 13],
  'INV': [1, 7],
  'C-': [1, 17],
  'A^Z': [1, 26],
  'SAA': [1, 41],
  'EDIT': [1, 8],
  'E^Z': [1, 24],
  'LOGZ': [1, 27],
  'TANH': [1, 35],
  '*FN': [1, 45],
  'ASA': [1, 40],
  'TANZ': [1, 32],
  'POLY': [1, 11],
  'VMAT': [1, 4],
  'MAGZ': [1, 20],
  'DIFEQ': [1, 14],
  'ROOTS': [1, 12],
  'C*': [1, 18],
  'SAS': [1, 42],
  'LNZ': [1, 25]
}; 
xroms['ILprinter'] = {
  'PRPLOT': [29, 14],
  'FMT': [29, 25],
  'PRREGX': [29, 17],
  'ACA': [29, 1],
  'PR~': [29, 18],
  'PRBUF': [29, 10],
  'REGPLOT': [29, 21],
  'SKPCOL': [29, 23],
  'ACSPEC': [29, 4],
  'PRSTK': [29, 19],
  'PRPLOTP': [29, 15],
  'PRA': [29, 8],
  'PRAXIS': [29, 9],
  'LIST': [29, 7],
  'PRKEYS': [29, 12],
  'ACCOL': [29, 3],
  'ACCHR': [29, 2],
  'BLDSPEC': [29, 6],
  'PRREG': [29, 16],
  'PRFLAGS': [29, 11],
  'ACX': [29, 5],
  'STKPLOT': [29, 24],
  'PRX': [29, 20],
  'SKPCHR': [29, 22],
  'PRP': [29, 13]
}; 
xroms['ILmass'] = {
  'READA': [28, 5],
  'WRTA': [28, 17],
  'PURGE': [28, 4],
  'WRTS': [28, 23],
  'VERIFY': [28, 16],
  'WRTR': [28, 21],
  'WRTP': [28, 19],
  'READR': [28, 8],
  'READS': [28, 10],
  'RENAME': [28, 12],
  'READP': [28, 7],
  'NEWM': [28, 3],
  'WRTK': [28, 18],
  'READRX': [28, 9],
  'UNSEC': [28, 15],
  'SEEKR': [28, 14],
  'READSUB': [28, 11],
  'WRTPV': [28, 20],
  'CREATE': [28, 1],
  'ZERO': [28, 24],
  'WRTRX': [28, 22],
  'DIR': [28, 2],
  'SEC': [28, 13],
  'READK': [28, 6]
}; 
xroms['ILcntl'] = {
  'REMOTE': [28, 38],
  'LISTEN': [28, 32],
  'IND': [28, 30],
  'AUTOIO': [28, 27],
  'MANIO': [28, 34],
  'LOCAL': [28, 33],
  'TRIGGER': [28, 41],
  'PWRUP': [28, 37],
  'INA': [28, 29],
  'PWRDN': [28, 36],
  'STOPIO': [28, 40],
  'SELECT': [28, 39],
  'INSTAT': [28, 31],
  'FINDID': [28, 28],
  'OUTA': [28, 35]
}; 
xroms['games'] = {
  'BOOM': [10, 9],
  'CRAPS': [10, 4],
  'BIOF': [10, 3],
  'P': [10, 11],
  'SIZE?': [10, 12],
  'INI': [10, 10],
  'BAGELS': [10, 1],
  'BIOR': [10, 2],
  'PINBALL': [10, 6],
  'HANG': [10, 5],
  'SWAR': [10, 7],
  'SUBHUNT': [10, 8],
  'RNDMW': [10, 14],
  'RNDM': [10, 13]
}; 
xroms['extfcn'] = {
  'REGSWAP': [25, 36],
  'GETR': [25, 19],
  'GETREC': [25, 20],
  'ATOX': [25, 7],
  'DELREC': [25, 13],
  'PURFL': [25, 31],
  'SAVEX': [25, 41],
  'PCLPS': [25, 27],
  'CRFLAS': [25, 10],
  'CLKEYS': [25, 9],
  'STOFLAG': [25, 45],
  'SAVER': [25, 39],
  'DELCHR': [25, 12],
  'RCLPTA': [25, 34],
  'EMDIR': [25, 14],
  'SAVEP': [25, 38],
  'INSREC': [25, 25],
  'SIZE?': [25, 44],
  'FLSIZE': [25, 15],
  'ARCLREC': [25, 5],
  'APPREC': [25, 4],
  'X<>F': [25, 46],
  'SAVEAS': [25, 37],
  'GETX': [25, 23],
  'RCLPT': [25, 33],
  'GETKEY': [25, 17],
  'CLFL': [25, 8],
  'SEEKPT': [25, 42],
  'GETAS': [25, 16],
  'GETSUB': [25, 22],
  'SEEKPTA': [25, 43],
  'APPCHR': [25, 3],
  'AROT': [25, 6],
  'SAVERX': [25, 40],
  'XTOA': [25, 47],
  'ALENG': [25, 1],
  'PASN': [25, 26],
  'REGMOVE': [25, 35],
  'POSFL': [25, 29],
  'INSCHR': [25, 24],
  'PSIZE': [25, 30],
  'POSA': [25, 28],
  'ANUM': [25, 2],
  'GETP': [25, 18],
  'CRFLD': [25, 11],
  'RCLFLAG': [25, 32],
  'GETRX': [25, 21]
}; 
xroms['CXtime'] = {
  'SETDATE': [26, 22],
  'CLALMA': [26, 31],
  'ATIME24': [26, 5],
  'ADATE': [26, 1],
  'TIME': [26, 27],
  'CLKTD': [26, 9],
  'T+X': [26, 26],
  'SW': [26, 25],
  'CLK12': [26, 6],
  'MDY': [26, 17],
  'SETAF': [26, 21],
  'DMY': [26, 15],
  'CLK24': [26, 7],
  'RCLSW': [26, 19],
  'CLALMX': [26, 32],
  'STOPSW': [26, 24],
  'DATE+': [26, 13],
  'CLOCK': [26, 10],
  'DOW': [26, 16],
  'XYZALM': [26, 28],
  'ALMNOW': [26, 3],
  'SETSW': [26, 23],
  'ATIME': [26, 4],
  'CLKT': [26, 8],
  'RCLALM': [26, 34],
  'CLRALMS': [26, 33],
  'RCLAF': [26, 18],
  'DATE': [26, 12],
  'SWPT': [26, 35],
  'RUNSW': [26, 20],
  'DDAYS': [26, 14],
  'CORRECT': [26, 11],
  'ALMCAT': [26, 2]
}; 
xroms['CXextfcn'] = {
  'REGSWAP': [25, 36],
  'ASROOM': [25, 49],
  'GETR': [25, 19],
  'GETREC': [25, 20],
  '~REG?': [25, 56],
  'ATOX': [25, 7],
  'DELREC': [25, 13],
  'PURFL': [25, 31],
  'SAVEX': [25, 41],
  'PCLPS': [25, 27],
  'CRFLAS': [25, 10],
  'ED': [25, 51],
  'CLKEYS': [25, 9],
  'STOFLAG': [25, 45],
  'SAVER': [25, 39],
  'DELCHR': [25, 12],
  'RESZFL': [25, 55],
  'RCLPTA': [25, 34],
  'EMDIR': [25, 14],
  'SAVEP': [25, 38],
  'CLRGX': [25, 50],
  'INSREC': [25, 25],
  'GETKEYX': [25, 54],
  'SIZE?': [25, 44],
  'FLSIZE': [25, 15],
  'ARCLREC': [25, 5],
  'APPREC': [25, 4],
  'X!=NN?': [25, 58],
  'X>NN?': [25, 61],
  'X<>F': [25, 46],
  'SAVEAS': [25, 37],
  'GETX': [25, 23],
  'RCLPT': [25, 33],
  'EMDIRX': [25, 52],
  'GETKEY': [25, 17],
  'X=NN?': [25, 57],
  'CLFL': [25, 8],
  'SEEKPT': [25, 42],
  'X<NN?': [25, 59],
  'GETAS': [25, 16],
  'GETSUB': [25, 22],
  'SEEKPTA': [25, 43],
  'APPCHR': [25, 3],
  'X<=NN?': [25, 60],
  'AROT': [25, 6],
  'SAVERX': [25, 40],
  'XTOA': [25, 47],
  'ALENG': [25, 1],
  'PASN': [25, 26],
  'EMROOM': [25, 53],
  'REGMOVE': [25, 35],
  'POSFL': [25, 29],
  'X>=NN?': [25, 62],
  'INSCHR': [25, 24],
  'PSIZE': [25, 30],
  'POSA': [25, 28],
  'ANUM': [25, 2],
  'GETP': [25, 18],
  'CRFLD': [25, 11],
  'RCLFLAG': [25, 32],
  'GETRX': [25, 21]
}; 
xroms['cardr'] = {
  'WPRV': [30, 9],
  'RDTA': [30, 2],
  'WALL': [30, 6],
  'WDTA': [30, 7],
  'RDTAX': [30, 3],
  'WSTS': [30, 10],
  'WDTAX': [30, 8],
  'RSUB': [30, 4],
  'VER': [30, 5],
  'MRG': [30, 1]
}; 
xroms['advantage'] = {
  'MSVS': [22, 50],
  'MSWAP': [22, 49],
  'MRR-': [22, 43],
  'V+': [24, 41],
  'MRR+': [22, 42],
  'AND': [22, 8],
  'C-': [24, 20],
  'MAGZ': [24, 8],
  'HEXVIEW': [22, 6],
  'YC+C': [22, 60],
  'MSR+': [22, 48],
  'MP': [22, 63],
  'I-': [22, 21],
  'PLV': [24, 24],
  'J-': [22, 23],
  'DSIG': [24, 29],
  'LOGZ': [24, 16],
  'MNAME?': [22, 36],
  'RSUM': [22, 56],
  'BINVIEW': [22, 2],
  'M*M': [22, 24],
  'Z^1/W': [24, 17],
  'MDET': [22, 32],
  'VXY': [24, 42],
  'V-': [24, 40],
  'FNRM': [22, 19],
  'RNRM': [22, 55],
  'MRIJ': [22, 40],
  'SIRIN': [24, 6],
  'BIT?': [22, 12],
  'C/': [24, 23],
  'FIT': [24, 31],
  'RIS': [24, 25],
  'ROTXY': [22, 11],
  'HEXIN': [22, 5],
  'MSIJ': [22, 46],
  'MR': [22, 37],
  'OR': [22, 9],
  'TR': [24, 47],
  'TVM': [24, 51],
  'Z^1/N': [24, 11],
  'BININ': [22, 1],
  'VR': [24, 37],
  'VD': [24, 45],
  'MAT/': [22, 28],
  'CT': [24, 48],
  'MAT-': [22, 27],
  'VANG': [24, 44],
  'MAT+': [22, 26],
  'C*': [24, 22],
  'SILOOP': [24, 5],
  'INTEG': [24, 4],
  'TANZ': [24, 14],
  'PIV': [22, 51],
  'MTR': [24, 1],
  'MINV': [22, 34],
  'N': [24, 52],
  'R<>R': [22, 52],
  'MRIJR': [22, 41],
  'V*': [24, 46],
  'LNZ': [24, 10],
  'FV': [24, 55],
  'Z^W': [24, 18],
  'CMEDIT': [22, 62],
  'MSIJR': [22, 47],
  'R>R?': [22, 53],
  'PV': [24, 53],
  'SOLVE': [24, 3],
  'PMT': [24, 54],
  'CROSS': [24, 35],
  'MRC-': [22, 39],
  'UV': [24, 43],
  'CMAXAB': [22, 15],
  'MRC+': [22, 38],
  'TRNPS': [22, 59],
  'DOT': [24, 38],
  'XOR': [22, 10],
  'MSC+': [22, 45],
  'DIFEQ': [24, 26],
  'NOT': [22, 7],
  'SZ?': [24, 33],
  'SINZ': [24, 12],
  'CVTVIEW': [22, 4],
  'SUM': [22, 57],
  'a^Z': [24, 15],
  'VC': [24, 34],
  'CINV': [24, 21],
  'C<>C': [22, 14],
  'MEDIT': [22, 61],
  'e^Z': [24, 9],
  'ASIG': [24, 28],
  'CSUM': [22, 17],
  'MAXAB': [22, 31],
  'CFIT': [24, 30],
  'MS': [22, 44],
  'OCTIN': [22, 3],
  'COSZ': [24, 13],
  'CNRM': [22, 16],
  'SUMAB': [22, 58],
  'MMOVE': [22, 35],
  'MAT*': [22, 25],
  'VS': [24, 36],
  'Y?X': [24, 32],
  'VE': [24, 39],
  '*I': [24, 56],
  'MIN': [22, 33],
  'MATDIM': [22, 29],
  'C+': [24, 19],
  'MATRX': [23, 0],
  'RMAXAB': [22, 54],
  'AIP': [24, 49],
  'MAX': [22, 30],
  'Z^N': [24, 7],
  'DIM?': [22, 18],
  'I+': [22, 20],
  'J+': [22, 22]
}; 
xroms['yfns'] = {
  'YGETLB': [31, 50],
  'YMCLR': [31, 14],
  'PLUG2L': [31, 23],
  'YPUTUB': [31, 53],
  'PLUG3L': [31, 26],
  'PLUG2U': [31, 24],
  'UPLUG3': [31, 37],
  'MMU?': [31, 4],
  'PLUG3U': [31, 27],
  'PLUG1L': [31, 20],
  'PLUG1U': [31, 21],
  'BAUD48': [31, 46],
  'TURBO50': [31, 10],
  'YSEC': [31, 56],
  'PLUG4L': [31, 29],
  'PLUGH': [31, 59],
  'PLUG4U': [31, 30],
  'TURBO10': [31, 8],
  'MMUCLR': [31, 1],
  'YMCPY': [31, 15],
  'PLUGP': [31, 57],
  'UPLUG2U': [31, 36],
  'UPLUG3L': [31, 38],
  'UPLUG4': [31, 40],
  'YFERASE': [31, 43],
  'TURBO2': [31, 6],
  'YBPNT': [31, 16],
  'BAUD96': [31, 45],
  'YPOKE': [31, 12],
  'TURBOX': [31, 5],
  'TURBO20': [31, 9],
  'TURBO?': [31, 11],
  'UPLUG4L': [31, 41],
  'UPLUG3U': [31, 39],
  'YEXP': [31, 54],
  'SERINI': [31, 49],
  'BAUD12': [31, 48],
  'MMUEN': [31, 3],
  'YBPNT?': [31, 17],
  'UPLUG1': [31, 31],
  'YFNS?': [31, 61],
  'UPLUGP': [31, 58],
  'PLUG2': [31, 22],
  'PLUG3': [31, 25],
  'PLUG1': [31, 19],
  'UPLUGH': [31, 60],
  'UPLUG4U': [31, 42],
  'PLUG4': [31, 28],
  'UPLUG1L': [31, 32],
  'YIMP': [31, 55],
  'YFWR': [31, 44],
  'YPEEK': [31, 13],
  'UPLUG2': [31, 34],
  'YBUILD': [31, 18],
  'BAUD24': [31, 47],
  'YPUTLB': [31, 52],
  'YGETUB': [31, 51],
  'MMUDIS': [31, 2],
  'UPLUG1U': [31, 33],
  'TURBO5': [31, 7],
  'UPLUG2L': [31, 35]
}; 
xroms['yfnz'] = {
  'YGETLB': [15, 50],
  'YMCLR': [15, 14],
  'PLUG2L': [15, 23],
  'YPUTUB': [15, 53],
  'PLUG3L': [15, 26],
  'PLUG2U': [15, 24],
  'UPLUG3': [15, 37],
  'MMU?': [15, 4],
  'PLUG3U': [15, 27],
  'PLUG1L': [15, 20],
  'PLUG1U': [15, 21],
  'BAUD48': [15, 46],
  'TURBO50': [15, 10],
  'YSEC': [15, 56],
  'PLUG4L': [15, 29],
  'PLUGH': [15, 59],
  'PLUG4U': [15, 30],
  'TURBO10': [15, 8],
  'MMUCLR': [15, 1],
  'YMCPY': [15, 15],
  'PLUGP': [15, 57],
  'UPLUG2U': [15, 36],
  'UPLUG3L': [15, 38],
  'UPLUG4': [15, 40],
  'YFERASE': [15, 43],
  'TURBO2': [15, 6],
  'YBPNT': [15, 16],
  'BAUD96': [15, 45],
  'YPOKE': [15, 12],
  'TURBOX': [15, 5],
  'TURBO20': [15, 9],
  'TURBO?': [15, 11],
  'UPLUG4L': [15, 41],
  'UPLUG3U': [15, 39],
  'YEXP': [15, 54],
  'SERINI': [15, 49],
  'BAUD12': [15, 48],
  'MMUEN': [15, 3],
  'YBPNT?': [15, 17],
  'UPLUG1': [15, 31],
  'YFNS?': [15, 61],
  'UPLUGP': [15, 58],
  'PLUG2': [15, 22],
  'PLUG3': [15, 25],
  'PLUG1': [15, 19],
  'UPLUGH': [15, 60],
  'UPLUG4U': [15, 42],
  'PLUG4': [15, 28],
  'UPLUG1L': [15, 32],
  'YIMP': [15, 55],
  'YFWR': [15, 44],
  'YPEEK': [15, 13],
  'UPLUG2': [15, 34],
  'YBUILD': [15, 18],
  'BAUD24': [15, 47],
  'YPUTLB': [15, 52],
  'YGETUB': [15, 51],
  'MMUDIS': [15, 2],
  'UPLUG1U': [15, 33],
  'TURBO5': [15, 7],
  'UPLUG2L': [15, 35]
}; 
