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
  'ASIN': 92,
  'RTN': 133,
  '%': 76,
  'X^2': 81,
  '10^X': 87,
  'AVIEW': 126,
  '*': 66,
  'AON': 140,
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
  'ADV': 143,
  '1/X': 96,
  'FACT': 98,
  'Y^X': 83,
  'COS': 90,
  'CHS': 84,
  '}OCT': 111,
  'ENTER^': 131,
  'X<>Y': 113,
  'NULL': 0,
  'MOD': 75,
  'OFF': 141,
  'LASTX': 118,
  'HMS-': 74,
  'ASHF': 136,
  'R}P': 79,
  'E^X-1': 88,
  'HMS+': 73,
  'CLST': 115,
  'ATAN': 94,
  'RAD': 129,
  'CLA': 135,
  'D}R': 106,
  'CLRG': 138,
  '}DEC': 95,
  'P}R': 78,
  '%CH': 77,
  '}HR': 109,
  'SIGN': 122,
  'X!=0?': 99,
  'BEEP': 134,
  'LOG': 86,
  'X<=0?': 123,
  'STOP': 132,
  'CLD': 127,
  'SIN': 89,
  'LN': 80,
  'X<0?': 102,
  'R}D': 107
}; 
var two_byte_builtins = {
  'SPARE1': 175,
  'CF': 169,
  'RCL': 144,
  'LBL2': 207,
  'SPARE2': 176,
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
var xroms = {
  'READA': [28, 5],
  'SKPCOL': [29, 23],
  'WALL': [30, 6],
  'WPRV': [30, 9],
  'GETX': [25, 23],
  'INSTAT': [28, 31],
  'VERIFY': [28, 16],
  'DATE+': [26, 13],
  'WNDDTX': [27, 2],
  'READR': [28, 8],
  'READS': [28, 10],
  'READP': [28, 7],
  'SEEKPT': [25, 42],
  'SEEKR': [28, 14],
  'CLOCK': [26, 10],
  'WDTA': [30, 7],
  'LOCAL': [28, 33],
  'VER': [30, 5],
  'PRX': [29, 20],
  'PCLPS': [25, 27],
  'IND': [28, 30],
  'REMOTE': [28, 38],
  'INA': [28, 29],
  'CLFL': [25, 8],
  'ALMNOW': [26, 3],
  'RUNSW': [26, 20],
  'ACCOL': [29, 3],
  'WNDTST': [27, 6],
  'DELCHR': [25, 12],
  'RCLAF': [26, 18],
  'CREATE': [28, 1],
  'DIR': [28, 2],
  'PRP': [29, 13],
  'PURGE': [28, 4],
  'ATOX': [25, 7],
  'SEC': [28, 13],
  'PRREG': [29, 16],
  'WNDSUB': [27, 4],
  'PURFL': [25, 31],
  'DMY': [26, 15],
  'MRG': [30, 1],
  'XYZALM': [26, 28],
  'PRKEYS': [29, 12],
  'PRAXIS': [29, 9],
  'GETKEYX': [25, 54],
  'X>NN?': [25, 61],
  'INSCHR': [25, 24],
  'RCLSW': [26, 19],
  'X<=NN?': [25, 60],
  'GETREC': [25, 20],
  'LIST': [29, 7],
  'FMT': [29, 25],
  'X=NN?': [25, 57],
  'WNDDTA': [27, 1],
  'PRA': [29, 8],
  'X<NN?': [25, 59],
  'MDY': [26, 17],
  'SIZE?': [25, 44],
  'X>=NN?': [25, 62],
  'CLK24': [26, 7],
  'GETR': [25, 19],
  'POSA': [25, 28],
  'CLALMA': [26, 31],
  'SETAF': [26, 21],
  'CLK12': [26, 6],
  'FLSIZE': [25, 15],
  'AROT': [25, 6],
  'PASN': [25, 26],
  'PWRDN': [28, 36],
  'EMROOM': [25, 53],
  'GETAS': [25, 16],
  'WDTAX': [30, 8],
  'ARCLREC': [25, 5],
  'RCLPT': [25, 33],
  'READSUB': [28, 11],
  'GETP': [25, 18],
  'CORRECT': [26, 11],
  'STOFLAG': [25, 45],
  'ALMCAT': [26, 2],
  'SEEKPTA': [25, 43],
  'ALENG': [25, 1],
  'PRBUF': [29, 10],
  'SAVEX': [25, 41],
  'REGSWAP': [25, 36],
  'STKPLOT': [29, 24],
  'WRTA': [28, 17],
  'ACCHR': [29, 2],
  'MANIO': [28, 34],
  'SAVER': [25, 39],
  'ZERO': [28, 24],
  'SAVEP': [25, 38],
  'ATIME24': [26, 5],
  'GETSUB': [25, 22],
  'ACX': [29, 5],
  'RDTA': [30, 2],
  'PRPLOT': [29, 14],
  'NEWM': [28, 3],
  'PWRUP': [28, 37],
  'CRFLD': [25, 11],
  'APPREC': [25, 4],
  'SETSW': [26, 23],
  'TRIGGER': [28, 41],
  'CLKTD': [26, 9],
  'CLRGX': [25, 50],
  'PRREGX': [29, 17],
  'WNDSCN': [27, 5],
  'STOPSW': [26, 24],
  'OUTA': [28, 35],
  'UNSEC': [28, 15],
  'GETRX': [25, 21],
  'SW': [26, 25],
  'PSIZE': [25, 30],
  'SKPCHR': [29, 22],
  'EMDIR': [25, 14],
  'WNDLNK': [27, 3],
  'WRTS': [28, 23],
  'ADATE': [26, 1],
  'WSTS': [30, 10],
  'EMDIRX': [25, 52],
  'SELECT': [28, 39],
  'ACA': [29, 1],
  'WRTR': [28, 21],
  'INSREC': [25, 25],
  'BLDSPEC': [29, 6],
  'REGPLOT': [29, 21],
  'CLKEYS': [25, 9],
  'RCLPTA': [25, 34],
  'X<>F': [25, 46],
  'PRSTK': [29, 19],
  'XTOA': [25, 47],
  'STOPIO': [28, 40],
  'GETKEY': [25, 17],
  'REGMOVE': [25, 35],
  'RCLALM': [26, 34],
  'CLKT': [26, 8],
  'ANUM': [25, 2],
  'FINDID': [28, 28],
  'RESZFL': [25, 55],
  'WRTP': [28, 19],
  'READRX': [28, 9],
  'PRFLAGS': [29, 11],
  'SETDATE': [26, 22],
  'CLALMX': [26, 32],
  'RDTAX': [30, 3],
  'DDAYS': [26, 14],
  'SAVERX': [25, 40],
  'SAVEAS': [25, 37],
  'DOW': [26, 16],
  'RCLFLAG': [25, 32],
  'AUTOIO': [28, 27],
  'PRPLOTP': [29, 15],
  'ED': [25, 51],
  'WRTRX': [28, 22],
  '~REG?': [25, 56],
  'ATIME': [26, 4],
  'CRFLAS': [25, 10],
  'APPCHR': [25, 3],
  'CLRALMS': [26, 33],
  'TIME': [26, 27],
  'ACSPEC': [29, 4],
  'POSFL': [25, 29],
  'WRTPV': [28, 20],
  'DELREC': [25, 13],
  'PR~': [29, 18],
  'SWPT': [26, 35],
  'ASROOM': [25, 49],
  'LISTEN': [28, 32],
  'X!=NN?': [25, 58],
  'RENAME': [28, 12],
  'WRTK': [28, 18],
  'DATE': [26, 12],
  'T+X': [26, 26],
  'READK': [28, 6],
  'RSUB': [30, 4]
}; 
