open! Te_bot
open Te_core
open Te_top
module T = Types

module X = Spec.Build(functor(Context: Spec.CONTEXT) -> struct
  open Context

  let s = variable_supply
  let (start, s) = variable s "start"
  let (ws, s) = variable s "ws"

  let (ident, s) = variable s "ident"
  let (string, s) = variable s "string"
  let (char, s) = variable s "char"
  let (integer, s) = variable s "integer"
  let (unsigned, s) = variable s "unsigned"
  let (long, s) = variable s "long"
  let (unsignedlong, s) = variable s "unsignedlong"
  let (hexadecimal, s) = variable s "hexadecimal"
  let (hexunsigned, s) = variable s "hexunsigned"
  let (hexlong, s) = variable s "hexlong"
  let (hexunslong, s) = variable s "hexunslong"
  let (octal, s) = variable s "octal"
  let (octalunsigned, s) = variable s "octalunsigned"
  let (octallong, s) = variable s "octallong"
  let (octalunslong, s) = variable s "octalunslong"
  let (double, s) = variable s "double"
  let (float, s) = variable s "float"
  let (longdouble, s) = variable s "longdouble"
  let (type_void, s) = variable s "type_void"
  let (type_char, s) = variable s "type_char"
  let (type_short, s) = variable s "type_short"
  let (type_int, s) = variable s "type_int"
  let (type_long, s) = variable s "type_long"
  let (type_float, s) = variable s "type_float"
  let (type_double, s) = variable s "type_double"
  let (type_signed, s) = variable s "type_signed"
  let (type_unsigned, s) = variable s "type_unsigned"
  let (type_typedef, s) = variable s "type_typedef"
  let (typedef, s) = variable s "typedef"
  let (extern, s) = variable s "extern"
  let (static, s) = variable s "static"
  let (auto, s) = variable s "auto"
  let (register, s) = variable s "register"
  let (const, s) = variable s "const"
  let (volatile, s) = variable s "volatile"
  let (struct_, s) = variable s "struct_"
  let (union, s) = variable s "union"
  let (enum, s) = variable s "enum"
  let (case, s) = variable s "case"
  let (default, s) = variable s "default"
  let (if_, s) = variable s "if_"
  let (else_, s) = variable s "else_"
  let (switch, s) = variable s "switch"
  let (while_, s) = variable s "while_"
  let (do_, s) = variable s "do_"
  let (for_, s) = variable s "for_"
  let (goto, s) = variable s "goto"
  let (continue, s) = variable s "continue"
  let (break, s) = variable s "break"
  let (return, s) = variable s "return"
  let (sizeof, s) = variable s "sizeof"
  let (ternary_if, s) = variable s "ternary_if"
  let (ternary_else, s) = variable s "ternary_else"
  let (address, s) = variable s "address"
  let (indirection, s) = variable s "indirection"
  let (pointer, s) = variable s "pointer"
  let (bitcomp, s) = variable s "bitcomp"
  let (lneg, s) = variable s "lneg"
  let (semi, s) = variable s "semi"
  let (lparen, s) = variable s "lparen"
  let (rparen, s) = variable s "rparen"
  let (comma, s) = variable s "comma"
  let (lbrace, s) = variable s "lbrace"
  let (rbrace, s) = variable s "rbrace"
  let (colon, s) = variable s "colon"
  let (lbrak, s) = variable s "lbrak"
  let (rbrak, s) = variable s "rbrak"
  let (assign, s) = variable s "assign"
  let (plus_, s) = variable s "plus_"
  let (minus, s) = variable s "minus"
  let (ellip, s) = variable s "ellip"
  let (bor, s) = variable s "bor"
  let (band, s) = variable s "band"
  let (lor_, s) = variable s "lor_"
  let (land_, s) = variable s "land_"
  let (bxor, s) = variable s "bxor"
  let (eq, s) = variable s "eq"
  let (noteq, s) = variable s "noteq"
  let (lt, s) = variable s "lt"
  let (gt, s) = variable s "gt"
  let (gte, s) = variable s "gte"
  let (lte, s) = variable s "lte"
  let (lshift, s) = variable s "lshift"
  let (rshift, s) = variable s "rshift"
  let (ast, s) = variable s "ast"
  let (slash, s) = variable s "slash"
  let (rem, s) = variable s "rem"
  let (plusplus, s) = variable s "plusplus"
  let (minusminus, s) = variable s "minusminus"
  let (dot, s) = variable s "dot"
  let (arrow, s) = variable s "arrow"
  let (astassign, s) = variable s "timesassign"
  let (slashassign, s) = variable s "divassign"
  let (remassign, s) = variable s "remassign"
  let (plusassign, s) = variable s "plusassign"
  let (minusassign, s) = variable s "minusassign"
  let (lshiftassign, s) = variable s "lshiftassign"
  let (rshiftassign, s) = variable s "rshiftassign"
  let (landassign, s) = variable s "landassign"
  let (lxorassign, s) = variable s "lxorassign"
  let (lorassign, s) = variable s "lorassign"

  let (program, s) = variable s "program"
  let (list_external_declaration, s) = variable s "list_external_declaration"
  let (external_declaration, s) = variable s "external_declaration"
  let (function_def, s) = variable s "function_def"
  let (dec, s) = variable s "dec"
  let (list_dec, s) = variable s "list_dec"
  let (list_declaration_specifier, s) = variable s "list_declaration_specifier"
  let (declaration_specifier, s) = variable s "declaration_specifier"
  let (list_init_declarator, s) = variable s "list_init_declarator"
  let (init_declarator, s) = variable s "init_declarator"
  let (type_specifier, s) = variable s "type_specifier"
  let (storage_class_specifier, s) = variable s "storage_class_specifier"
  let (type_qualifier, s) = variable s "type_qualifier"
  let (struct_or_union_spec, s) = variable s "struct_or_union_spec"
  let (struct_or_union, s) = variable s "struct_or_union"
  let (list_struct_dec, s) = variable s "list_struct_dec"
  let (struct_dec, s) = variable s "struct_dec"
  let (list_spec_qual, s) = variable s "list_spec_qual"
  let (spec_qual, s) = variable s "spec_qual"
  let (list_struct_declarator, s) = variable s "list_struct_declarator"
  let (struct_declarator, s) = variable s "struct_declarator"
  let (enum_specifier, s) = variable s "enum_specifier"
  let (list_enumerator, s) = variable s "list_enumerator"
  let (enumerator, s) = variable s "enumerator"
  let (declarator, s) = variable s "declarator"
  let (direct_declarator, s) = variable s "direct_declarator"
  let (list_type_qualifier, s) = variable s "list_type_qualifier"
  let (parameter_type, s) = variable s "parameter_type"
  let (parameter_declarations, s) = variable s "parameter_declarations"
  let (parameter_declaration, s) = variable s "parameter_declaration"
  let (list_ident, s) = variable s "list_ident"
  let (initializer_, s) = variable s "initializer_"
  let (initializers, s) = variable s "initializers"
  let (type_name, s) = variable s "type_name"
  let (abstract_declarator, s) = variable s "abstract_declarator"
  let (dir_abs_dec, s) = variable s "dir_abs_dec"
  let (stm, s) = variable s "stm"
  let (labeled_stm, s) = variable s "labeled_stm"
  let (compound_stm, s) = variable s "compound_stm"
  let (expression_stm, s) = variable s "expression_stm"
  let (selection_stm, s) = variable s "selection_stm"
  let (iter_stm, s) = variable s "iter_stm"
  let (jump_stm, s) = variable s "jump_stm"
  let (list_stm, s) = variable s "list_stm"
  let (constant, s) = variable s "constant"
  let (constant_expression, s) = variable s "constant_expression"
  let (list_exp2, s) = variable s "list_exp2"
  let (exp, s) = variable s "exp"
  let (exp2, s) = variable s "exp2"
  let (exp3, s) = variable s "exp3"
  let (exp4, s) = variable s "exp4"
  let (exp5, s) = variable s "exp5"
  let (exp6, s) = variable s "exp6"
  let (exp7, s) = variable s "exp7"
  let (exp8, s) = variable s "exp8"
  let (exp9, s) = variable s "exp9"
  let (exp10, s) = variable s "exp10"
  let (exp11, s) = variable s "exp11"
  let (exp12, s) = variable s "exp12"
  let (exp13, s) = variable s "exp13"
  let (exp14, s) = variable s "exp14"
  let (exp15, s) = variable s "exp15"
  let (exp16, s) = variable s "exp16"
  let (exp17, s) = variable s "exp17"
  let (unary_operator, s) = variable s "unary_operator"
  let (assignment_op, s) = variable s "assignment_op"

  let (progr', s) = variable s "progr'"
  let (afunc', s) = variable s "afunc'"
  let (global', s) = variable s "global'"
  let (oldfunc', s) = variable s "oldfunc'"
  let (newfunc', s) = variable s "newfunc'"
  let (oldfuncint', s) = variable s "oldfuncint'"
  let (newfuncint', s) = variable s "newfuncint'"
  let (nodeclarator', s) = variable s "nodeclarator'"
  let (declarators', s) = variable s "declarators'"
  let (type', s) = variable s "type'"
  let (storage', s) = variable s "storage'"
  let (specprop', s) = variable s "specprop'"
  let (onlydecl', s) = variable s "onlydecl'"
  let (initdecl', s) = variable s "initdecl'"
  let (tvoid', s) = variable s "tvoid'"
  let (tchar', s) = variable s "tchar'"
  let (tshort', s) = variable s "tshort'"
  let (tint', s) = variable s "tint'"
  let (tlong', s) = variable s "tlong'"
  let (tfloat', s) = variable s "tfloat'"
  let (tdouble', s) = variable s "tdouble'"
  let (tsigned', s) = variable s "tsigned'"
  let (tunsigned', s) = variable s "tunsigned'"
  let (tstruct', s) = variable s "tstruct'"
  let (tenum', s) = variable s "tenum'"
  let (tname', s) = variable s "tname'"
  let (mytype', s) = variable s "mytype'"
  let (globalprograms', s) = variable s "globalprograms'"
  let (localprogram', s) = variable s "localprogram'"
  let (localblock', s) = variable s "localblock'"
  let (localreg', s) = variable s "localreg'"
  let (const', s) = variable s "const'"
  let (nooptim', s) = variable s "nooptim'"
  let (tag', s) = variable s "tag'"
  let (unique', s) = variable s "unique'"
  let (tagtype', s) = variable s "tagtype'"
  let (struct', s) = variable s "struct'"
  let (union', s) = variable s "union'"
  let (structen', s) = variable s "structen'"
  let (typespec', s) = variable s "typespec'"
  let (qualspec', s) = variable s "qualspec'"
  let (decl', s) = variable s "decl'"
  let (field', s) = variable s "field'"
  let (decfield', s) = variable s "decfield'"
  let (enumdec', s) = variable s "enumdec'"
  let (enumname', s) = variable s "enumname'"
  let (enumvar', s) = variable s "enumvar'"
  let (plain', s) = variable s "plain'"
  let (enuminit', s) = variable s "enuminit'"
  let (beginpointer', s) = variable s "beginpointer'"
  let (nopointer', s) = variable s "nopointer'"
  let (name', s) = variable s "name'"
  let (parendecl', s) = variable s "parendecl'"
  let (innitarray', s) = variable s "innitarray'"
  let (incomplete', s) = variable s "incomplete'"
  let (newfuncdec', s) = variable s "newfuncdec'"
  let (oldfuncdef', s) = variable s "oldfuncdef'"
  let (oldfuncdec', s) = variable s "oldfuncdec'"
  let (point', s) = variable s "point'"
  let (pointqual', s) = variable s "pointqual'"
  let (pointpoint', s) = variable s "pointpoint'"
  let (pointqualpoint', s) = variable s "pointqualpoint'"
  let (allspec', s) = variable s "allspec'"
  let (more', s) = variable s "more'"
  let (paramdec', s) = variable s "paramdec'"
  let (moreparamdec', s) = variable s "moreparamdec'"
  let (onlytype', s) = variable s "onlytype'"
  let (typeandparam', s) = variable s "typeandparam'"
  let (abstract', s) = variable s "abstract'"
  let (initexpr', s) = variable s "initexpr'"
  let (initlistone', s) = variable s "initlistone'"
  let (initlisttwo', s) = variable s "initlisttwo'"
  let (aninit', s) = variable s "aninit'"
  let (moreinit', s) = variable s "moreinit'"
  let (plaintype', s) = variable s "plaintype'"
  let (extendedtype', s) = variable s "extendedtype'"
  let (pointerstart', s) = variable s "pointerstart'"
  let (advanced', s) = variable s "advanced'"
  let (pointadvanced', s) = variable s "pointadvanced'"
  let (withinparentes', s) = variable s "withinparentes'"
  let (array', s) = variable s "array'"
  let (initiatedarray', s) = variable s "initiatedarray'"
  let (uninitiated', s) = variable s "uninitiated'"
  let (initiated', s) = variable s "initiated'"
  let (oldfunction', s) = variable s "oldfunction'"
  let (newfunction', s) = variable s "newfunction'"
  let (oldfuncexpr', s) = variable s "oldfuncexpr'"
  let (newfuncexpr', s) = variable s "newfuncexpr'"
  let (labels', s) = variable s "labels'"
  let (comps', s) = variable s "comps'"
  let (exprs', s) = variable s "exprs'"
  let (sels', s) = variable s "sels'"
  let (iters', s) = variable s "iters'"
  let (jumps', s) = variable s "jumps'"
  let (slabelone', s) = variable s "slabelone'"
  let (slabeltwo', s) = variable s "slabeltwo'"
  let (slabelthree', s) = variable s "slabelthree'"
  let (scompone', s) = variable s "scompone'"
  let (scomptwo', s) = variable s "scomptwo'"
  let (scompthree', s) = variable s "scompthree'"
  let (scompfour', s) = variable s "scompfour'"
  let (sexprone', s) = variable s "sexprone'"
  let (sexprtwo', s) = variable s "sexprtwo'"
  let (sselone', s) = variable s "sselone'"
  let (sseltwo', s) = variable s "sseltwo'"
  let (sselthree', s) = variable s "sselthree'"
  let (siterone', s) = variable s "siterone'"
  let (sitertwo', s) = variable s "sitertwo'"
  let (siterthree', s) = variable s "siterthree'"
  let (siterfour', s) = variable s "siterfour'"
  let (sjumpone', s) = variable s "sjumpone'"
  let (sjumptwo', s) = variable s "sjumptwo'"
  let (sjumpthree', s) = variable s "sjumpthree'"
  let (sjumpfour', s) = variable s "sjumpfour'"
  let (sjumpfive', s) = variable s "sjumpfive'"
  let (ecomma', s) = variable s "ecomma'"
  let (eassign', s) = variable s "eassign'"
  let (econdition', s) = variable s "econdition'"
  let (elor', s) = variable s "elor'"
  let (eland', s) = variable s "eland'"
  let (ebitor', s) = variable s "ebitor'"
  let (ebitexor', s) = variable s "ebitexor'"
  let (ebitand', s) = variable s "ebitand'"
  let (eeq', s) = variable s "eeq'"
  let (eneq', s) = variable s "eneq'"
  let (elthen', s) = variable s "elthen'"
  let (egrthen', s) = variable s "egrthen'"
  let (ele', s) = variable s "ele'"
  let (ege', s) = variable s "ege'"
  let (eleft', s) = variable s "eleft'"
  let (eright', s) = variable s "eright'"
  let (eplus', s) = variable s "eplus'"
  let (eminus', s) = variable s "eminus'"
  let (etimes', s) = variable s "etimes'"
  let (ediv', s) = variable s "ediv'"
  let (emod', s) = variable s "emod'"
  let (etypeconv', s) = variable s "etypeconv'"
  let (epreinc', s) = variable s "epreinc'"
  let (epredec', s) = variable s "epredec'"
  let (epreop', s) = variable s "epreop'"
  let (ebytesexpr', s) = variable s "ebytesexpr'"
  let (ebytestype', s) = variable s "ebytestype'"
  let (earray', s) = variable s "earray'"
  let (efunk', s) = variable s "efunk'"
  let (efunkpar', s) = variable s "efunkpar'"
  let (eselect', s) = variable s "eselect'"
  let (epoint', s) = variable s "epoint'"
  let (epostinc', s) = variable s "epostinc'"
  let (epostdec', s) = variable s "epostdec'"
  let (evar', s) = variable s "evar'"
  let (econst', s) = variable s "econst'"
  let (estring', s) = variable s "estring'"
  let (echar', s) = variable s "echar'"
  let (eunsigned', s) = variable s "eunsigned'"
  let (elong', s) = variable s "elong'"
  let (eunsignlong', s) = variable s "eunsignlong'"
  let (ehexadec', s) = variable s "ehexadec'"
  let (ehexaunsign', s) = variable s "ehexaunsign'"
  let (ehexalong', s) = variable s "ehexalong'"
  let (ehexaunslong', s) = variable s "ehexaunslong'"
  let (eoctal', s) = variable s "eoctal'"
  let (eoctalunsign', s) = variable s "eoctalunsign'"
  let (eoctallong', s) = variable s "eoctallong'"
  let (eoctalunslong', s) = variable s "eoctalunslong'"
  let (edouble', s) = variable s "edouble'"
  let (efloat', s) = variable s "efloat'"
  let (elongdouble', s) = variable s "elongdouble'"
  let (eint', s) = variable s "eint'"
  let (especial', s) = variable s "especial'"
  let (address', s) = variable s "address'"
  let (indirection', s) = variable s "indirection'"
  let (plus', s) = variable s "plus'"
  let (negative', s) = variable s "negative'"
  let (complement', s) = variable s "complement'"
  let (logicalneg', s) = variable s "logicalneg'"
  let (assign', s) = variable s "assign'"
  let (assignmul', s) = variable s "assignmul'"
  let (assigndiv', s) = variable s "assigndiv'"
  let (assignmod', s) = variable s "assignmod'"
  let (assignadd', s) = variable s "assignadd'"
  let (assignsub', s) = variable s "assignsub'"
  let (assignleft', s) = variable s "assignleft'"
  let (assignright', s) = variable s "assignright'"
  let (assignand', s) = variable s "assignand'"
  let (assignxor', s) = variable s "assignxor'"
  let (assignor', s) = variable s "assignor'"

  let (u, s) = variable s "u"

  let syntactic = [
    start;
    program;
    list_external_declaration;
    external_declaration;
    function_def;
    dec;
    list_dec;
    list_declaration_specifier;
    declaration_specifier;
    list_init_declarator;
    init_declarator;
    type_specifier;
    storage_class_specifier;
    type_qualifier;
    struct_or_union_spec;
    struct_or_union;
    list_struct_dec;
    struct_dec;
    list_spec_qual;
    spec_qual;
    list_struct_declarator;
    struct_declarator;
    enum_specifier;
    list_enumerator;
    enumerator;
    declarator;
    direct_declarator;
    list_type_qualifier;
    parameter_type;
    parameter_declarations;
    parameter_declaration;
    list_ident;
    initializer_;
    initializers;
    type_name;
    abstract_declarator;
    dir_abs_dec;
    stm;
    labeled_stm;
    compound_stm;
    expression_stm;
    selection_stm;
    iter_stm;
    jump_stm;
    list_stm;
    constant;
    constant_expression;
    list_exp2;
    exp;
    exp2;
    exp3;
    exp4;
    exp5;
    exp6;
    exp7;
    exp8;
    exp9;
    exp10;
    exp11;
    exp12;
    exp13;
    exp14;
    exp15;
    exp16;
    exp17;
    unary_operator;
    assignment_op;
    pointer;
  ]
  let lexical = [
    ident;
    string;
    char;
    integer;
    unsigned;
    long;
    unsignedlong;
    hexadecimal;
    hexunsigned;
    hexlong;
    hexunslong;
    octal;
    octalunsigned;
    octallong;
    octalunslong;
    double;
    float;
    longdouble;
    type_void;
    type_char;
    type_short;
    type_int;
    type_long;
    type_float;
    type_double;
    type_signed;
    type_unsigned;
    type_typedef;
    typedef;
    extern;
    static;
    auto;
    register;
    const;
    volatile;
    struct_;
    union;
    enum;
    case;
    default;
    if_;
    else_;
    switch;
    while_;
    do_;
    for_;
    goto;
    continue;
    break;
    return;
    sizeof;
    ternary_if;
    ternary_else;
    address;
    indirection;
    bitcomp;
    lneg;
    semi;
    lparen;
    rparen;
    comma;
    lbrace;
    rbrace;
    colon;
    lbrak;
    rbrak;
    assign;
    plus_;
    minus;
    ellip;
    bor;
    band;
    lor_;
    land_;
    bxor;
    eq;
    noteq;
    lt;
    gt;
    gte;
    lte;
    lshift;
    rshift;
    ast;
    slash;
    rem;
    plusplus;
    minusminus;
    dot;
    arrow;
    astassign;
    slashassign;
    remassign;
    plusassign;
    minusassign;
    lshiftassign;
    rshiftassign;
    landassign;
    lxorassign;
    lorassign;
    ws;
  ]

  let labels = [
    progr';
    afunc';
    global';
    oldfunc';
    newfunc';
    oldfuncint';
    newfuncint';
    nodeclarator';
    declarators';
    type';
    storage';
    specprop';
    onlydecl';
    initdecl';
    tvoid';
    tchar';
    tshort';
    tint';
    tlong';
    tfloat';
    tdouble';
    tsigned';
    tunsigned';
    tstruct';
    tenum';
    tname';
    mytype';
    globalprograms';
    localprogram';
    localblock';
    localreg';
    const';
    nooptim';
    tag';
    unique';
    tagtype';
    struct';
    union';
    structen';
    typespec';
    qualspec';
    decl';
    field';
    decfield';
    enumdec';
    enumname';
    enumvar';
    plain';
    enuminit';
    beginpointer';
    nopointer';
    name';
    parendecl';
    innitarray';
    incomplete';
    newfuncdec';
    oldfuncdef';
    oldfuncdec';
    point';
    pointqual';
    pointpoint';
    pointqualpoint';
    allspec';
    more';
    paramdec';
    moreparamdec';
    onlytype';
    typeandparam';
    abstract';
    initexpr';
    initlistone';
    initlisttwo';
    aninit';
    moreinit';
    plaintype';
    extendedtype';
    pointerstart';
    advanced';
    pointadvanced';
    withinparentes';
    array';
    initiatedarray';
    uninitiated';
    initiated';
    oldfunction';
    newfunction';
    oldfuncexpr';
    newfuncexpr';
    labels';
    comps';
    exprs';
    sels';
    iters';
    jumps';
    slabelone';
    slabeltwo';
    slabelthree';
    scompone';
    scomptwo';
    scompthree';
    scompfour';
    sexprone';
    sexprtwo';
    sselone';
    sseltwo';
    sselthree';
    siterone';
    sitertwo';
    siterthree';
    siterfour';
    sjumpone';
    sjumptwo';
    sjumpthree';
    sjumpfour';
    sjumpfive';
    ecomma';
    eassign';
    econdition';
    elor';
    eland';
    ebitor';
    ebitexor';
    ebitand';
    eeq';
    eneq';
    elthen';
    egrthen';
    ele';
    ege';
    eleft';
    eright';
    eplus';
    eminus';
    etimes';
    ediv';
    emod';
    etypeconv';
    epreinc';
    epredec';
    epreop';
    ebytesexpr';
    ebytestype';
    earray';
    efunk';
    efunkpar';
    eselect';
    epoint';
    epostinc';
    epostdec';
    evar';
    econst';
    estring';
    echar';
    eunsigned';
    elong';
    eunsignlong';
    ehexadec';
    ehexaunsign';
    ehexalong';
    ehexaunslong';
    eoctal';
    eoctalunsign';
    eoctallong';
    eoctalunslong';
    edouble';
    efloat';
    elongdouble';
    eint';
    especial';
    address';
    indirection';
    plus';
    negative';
    complement';
    logicalneg';
    assign';
    assignmul';
    assigndiv';
    assignmod';
    assignadd';
    assignsub';
    assignleft';
    assignright';
    assignand';
    assignxor';
    assignor';
    u;
  ]

  let longest_match = [
    ident;
    ws;
    type_typedef;
  ]

  let start = start

  let parser =
    with_ws (T.Vars.of_list lexical) (var ws) @@ unextend s u Production.[
      make (u, start) R.(var program * plus eof);

      make (progr', program) R.(var ws * var list_external_declaration);

      make (u, list_external_declaration) (var external_declaration);
      make (u, list_external_declaration) R.(var external_declaration * var list_external_declaration);

      make (afunc', external_declaration) (var function_def);
      make (global', external_declaration) (var dec);

      make (oldfunc', function_def) R.(var list_declaration_specifier * var declarator * var list_dec * var compound_stm);
      make (newfunc', function_def) R.(var list_declaration_specifier * var declarator * var compound_stm);
      make (oldfuncint', function_def) R.(var declarator * var list_dec * var compound_stm);
      make (newfuncint', function_def) R.(var declarator * var compound_stm);

      make (nodeclarator', dec) R.(var list_declaration_specifier * var semi);
      make (declarators', dec) R.(var list_declaration_specifier * var list_init_declarator * var semi);

      make (u, list_dec) (var dec);
      make (u, list_dec) R.(var dec * var list_dec);

      make (u, list_declaration_specifier) (var declaration_specifier);
      make (u, list_declaration_specifier) R.(var declaration_specifier * var list_declaration_specifier);

      make (type', declaration_specifier) (var type_specifier);
      make (storage', declaration_specifier) (var storage_class_specifier);
      make (specprop', declaration_specifier) (var type_qualifier);

      make (u, list_init_declarator) (var init_declarator);
      make (u, list_init_declarator) R.(var init_declarator * var comma * var list_init_declarator);

      make (onlydecl', init_declarator) (var declarator);
      make (initdecl', init_declarator) R.(var declarator * var assign * var initializer_);

      make (tvoid', type_specifier) (var type_void);
      make (tchar', type_specifier) (var type_char);
      make (tshort', type_specifier) (var type_short);
      make (tint', type_specifier) (var type_int);
      make (tlong', type_specifier) (var type_long);
      make (tfloat', type_specifier) (var type_float);
      make (tdouble', type_specifier) (var type_double);
      make (tsigned', type_specifier) (var type_signed);
      make (tunsigned', type_specifier) (var type_unsigned);
      make (tstruct', type_specifier) (var struct_or_union_spec);
      make (tenum', type_specifier) (var enum_specifier);
      make (tname', type_specifier) (var type_typedef);

      make (mytype', storage_class_specifier) (var typedef);
      make (globalprograms', storage_class_specifier) (var extern);
      make (localprogram', storage_class_specifier) (var static);
      make (localblock', storage_class_specifier) (var auto);
      make (localreg', storage_class_specifier) (var register);

      make (const', type_qualifier) (var const);
      make (nooptim', type_qualifier) (var volatile);

      make (tag', struct_or_union_spec) R.(var struct_or_union * var ident * var lbrace * var list_struct_dec * var rbrace);
      make (unique', struct_or_union_spec) R.(var struct_or_union * var lbrace * var list_struct_dec * var rbrace);
      make (tagtype', struct_or_union_spec) R.(var struct_or_union * var ident);

      make (struct', struct_or_union) (var struct_);
      make (union', struct_or_union) (var union);

      make (u, list_struct_dec) (var struct_dec);
      make (u, list_struct_dec) R.(var struct_dec * var list_struct_dec);

      make (structen', struct_dec) R.(var list_spec_qual * var list_struct_declarator * var semi);

      make (u, list_spec_qual) (var spec_qual);
      make (u, list_spec_qual) R.(var spec_qual * var list_spec_qual);

      make (typespec', spec_qual) (var type_specifier);
      make (qualspec', spec_qual) (var type_qualifier);

      make (u, list_struct_declarator) (var struct_declarator);
      make (u, list_struct_declarator) R.(var struct_declarator * var comma * var list_struct_declarator);

      make (decl', struct_declarator) (var declarator);
      make (field', struct_declarator) R.(var colon * var constant_expression);
      make (decfield', struct_declarator) R.(var declarator * var colon * var constant_expression);


      make (enumdec', enum_specifier) R.(var enum * var lbrace * var list_enumerator * var rbrace);
      make (enumname', enum_specifier) R.(var enum * var ident * var lbrace * var list_enumerator * var rbrace);
      make (enumvar', enum_specifier) R.(var enum * var ident);

      make (u, list_enumerator) (var enumerator);
      make (u, list_enumerator) R.(var enumerator * var comma);
      make (u, list_enumerator) R.(var enumerator * var comma * var list_enumerator);

      make (plain', enumerator) (var ident);
      make (enuminit', enumerator) R.(var ident * var assign * var constant_expression);


      make (beginpointer', declarator) R.(var pointer * var direct_declarator);
      make (nopointer', declarator) (var direct_declarator);

      make (name', direct_declarator) (var ident);
      make (parendecl', direct_declarator) R.(var lparen * var declarator * var rparen);
      make (innitarray', direct_declarator) R.(var direct_declarator * var lbrak * var constant_expression * var rbrak);
      make (incomplete', direct_declarator) R.(var direct_declarator * var lbrak * var rbrak);
      make (newfuncdec', direct_declarator) R.(var direct_declarator * var lparen * var parameter_type * var rparen);
      make (oldfuncdef', direct_declarator) R.(var direct_declarator * var lparen * var list_ident * var rparen);
      make (oldfuncdec', direct_declarator) R.(var direct_declarator * var lparen * var rparen);

      make (point', pointer) (var ast);
      make (pointqual', pointer) R.(var ast * var list_type_qualifier);
      make (pointpoint', pointer) R.(var ast * var pointer);
      make (pointqualpoint', pointer) R.(var ast * var list_type_qualifier * var pointer);

      make (u, list_type_qualifier) (var type_qualifier);
      make (u, list_type_qualifier) R.(var type_qualifier * var list_type_qualifier);

      make (allspec', parameter_type) (var parameter_declarations);
      make (more', parameter_type) R.(var parameter_declarations * var comma * var ellip);

      make (paramdec', parameter_declarations) (var parameter_declaration);
      make (moreparamdec', parameter_declarations) R.(var parameter_declarations * var comma * var parameter_declaration);

      make (onlytype', parameter_declaration) (var list_declaration_specifier);
      make (typeandparam', parameter_declaration) R.(var list_declaration_specifier * var declarator);
      make (abstract', parameter_declaration) R.(var list_declaration_specifier * var abstract_declarator);

      make (u, list_ident) (var ident);
      make (u, list_ident) R.(var ident * var comma * var list_ident);

      make (initexpr', initializer_) (var exp2);
      make (initlistone', initializer_) R.(var lbrace * var initializers * var rbrace);
      make (initlisttwo', initializer_) R.(var lbrace * var initializers * var comma * var rbrace);

      make (aninit', initializers) (var initializer_);
      make (moreinit', initializers) R.(var initializers * var comma * var initializer_);

      make (plaintype', type_name) (var list_spec_qual);
      make (extendedtype', type_name) R.(var list_spec_qual * var abstract_declarator);

      make (pointerstart', abstract_declarator) (var pointer);
      make (advanced', abstract_declarator) (var dir_abs_dec);
      make (pointadvanced', abstract_declarator) R.(var pointer * var dir_abs_dec);

      make (withinparentes', dir_abs_dec) R.(var lparen * var abstract_declarator * var rparen);
      make (array', dir_abs_dec) R.(var lbrak * var rbrak);
      make (initiatedarray', dir_abs_dec) R.(var lbrak * var constant_expression * var rbrak);
      make (uninitiated', dir_abs_dec) R.(var dir_abs_dec * var lbrak * var rbrak);
      make (initiated', dir_abs_dec) R.(var dir_abs_dec * var lbrak * var constant_expression * var rbrak);
      make (oldfunction', dir_abs_dec) R.(var lparen * var rparen);
      make (newfunction', dir_abs_dec) R.(var lparen * var parameter_type * var rparen);
      make (oldfuncexpr', dir_abs_dec) R.(var dir_abs_dec * var lparen * var rparen);
      make (newfuncexpr', dir_abs_dec) R.(var dir_abs_dec * var lparen * var parameter_type * var rparen);

      make (labels', stm) (var labeled_stm);
      make (comps', stm) (var compound_stm);
      make (exprs', stm) (var expression_stm);
      make (sels', stm) (var selection_stm);
      make (iters', stm) (var iter_stm);
      make (jumps', stm) (var jump_stm);

      make (slabelone', labeled_stm) R.(var ident * var colon * var stm);
      make (slabeltwo', labeled_stm) R.(var case * var constant_expression * var colon * var stm);
      make (slabelthree', labeled_stm) R.(var default * var colon * var stm);

      make (scompone', compound_stm) R.(var lbrace * var rbrace);
      make (scomptwo', compound_stm) R.(var lbrace * var list_stm * var rbrace);
      make (scompthree', compound_stm) R.(var lbrace * var list_dec * var rbrace);
      make (scompfour', compound_stm) R.(var lbrace * var list_dec * var list_stm * var rbrace);

      make (sexprone', expression_stm) (var semi);
      make (sexprtwo', expression_stm) R.(var exp * var semi);

      make (sselone', selection_stm) R.(var if_ * var lparen * var exp * var rparen * var stm);
      make (sseltwo', selection_stm) R.(var if_ * var lparen * var exp * var rparen * var stm * var else_ * var stm);
      make (sselthree', selection_stm) R.(var switch * var lparen * var exp * var rparen * var stm);

      make (siterone', iter_stm) R.(var while_ * var lparen * var exp * var rparen * var stm);
      make (sitertwo', iter_stm) R.(var do_ * var stm * var while_ * var lparen * var exp * var rparen * var semi);
      make (siterthree', iter_stm) R.(var for_ * var lparen * var expression_stm * var expression_stm * var rparen * var stm);
      make (siterfour', iter_stm) R.(var for_ * var lparen * var expression_stm * var expression_stm * var exp * var rparen * var stm);

      make (sjumpone', jump_stm) R.(var goto * var ident * var semi);
      make (sjumptwo', jump_stm) R.(var continue * var semi);
      make (sjumpthree', jump_stm) R.(var break * var semi);
      make (sjumpfour', jump_stm) R.(var return * var semi);
      make (sjumpfive', jump_stm) R.(var return * var exp * var semi);

      make (u, list_stm) (var stm);
      make (u, list_stm) R.(var stm * var list_stm);

      make (ecomma', exp) R.(var exp * var comma * var exp2);
      make (eassign', exp2) R.(var exp15 * var assignment_op * var exp2);
      make (econdition', exp3) R.(var exp4 * var ternary_if * var exp * var ternary_else * var exp3);
      make (elor', exp4) R.(var exp4 * var lor_ * var exp5);
      make (eland', exp5) R.(var exp5 * var land_ * var exp6);
      make (ebitor', exp6) R.(var exp6 * var bor * var exp7);
      make (ebitexor', exp7) R.(var exp7 * var bxor * var exp8);
      make (ebitand', exp8) R.(var exp8 * var band * var exp9);
      make (eeq', exp9) R.(var exp9 * var eq * var exp10);
      make (eneq', exp9) R.(var exp9 * var noteq * var exp10);
      make (elthen', exp10) R.(var exp10 * var lt * var exp11);
      make (egrthen', exp10) R.(var exp10 * var gt * var exp11);
      make (ele', exp10) R.(var exp10 * var lte * var exp11);
      make (ege', exp10) R.(var exp10 * var gte * var exp11);
      make (eleft', exp11) R.(var exp11 * var lshift * var exp12);
      make (eright', exp11) R.(var exp11 * var rshift * var exp12);
      make (eplus', exp12) R.(var exp12 * var plus_ * var exp13);
      make (eminus', exp12) R.(var exp12 * var minus * var exp13);
      make (etimes', exp13) R.(var exp13 * var ast * var exp14);
      make (ediv', exp13) R.(var exp13 * var slash * var exp14);
      make (emod', exp13) R.(var exp13 * var rem * var exp14);
      make (etypeconv', exp14) R.(var lparen * var type_name * var rparen * var exp14);
      make (epreinc', exp15) R.(var plusplus * var exp15);
      make (epredec', exp15) R.(var minusminus * var exp15);
      make (epreop', exp15) R.(var unary_operator * var exp14);
      make (ebytesexpr', exp15) R.(var sizeof * var exp15);
      make (ebytestype', exp15) R.(var sizeof * var lparen * var type_name * var rparen);
      make (earray', exp16) R.(var exp16 * var lbrak * var exp * var rbrak);
      make (efunk', exp16) R.(var exp16 * var lparen * var rparen);
      make (efunkpar', exp16) R.(var exp16 * var lparen * var list_exp2 * var rparen);
      make (eselect', exp16) R.(var exp16 * var dot * var ident);
      make (epoint', exp16) R.(var exp16 * var arrow * var ident);
      make (epostinc', exp16) R.(var exp16 * var plusplus);
      make (epostdec', exp16) R.(var exp16 * var minusminus);
      make (evar', exp17) (var ident);
      make (econst', exp17) (var constant);
      make (estring', exp17) (var string);

      make (echar', constant) (var char);
      make (eunsigned', constant) (var unsigned);
      make (elong', constant) (var long);
      make (eunsignlong', constant) (var unsignedlong);
      make (ehexadec', constant) (var hexadecimal);
      make (ehexaunsign', constant) (var hexunsigned);
      make (ehexalong', constant) (var hexlong);
      make (ehexaunslong', constant) (var hexunslong);
      make (eoctal', constant) (var octal);
      make (eoctalunsign', constant) (var octalunsigned);
      make (eoctallong', constant) (var octallong);
      make (eoctalunslong', constant) (var octalunslong);
      make (edouble', constant) (var double);
      make (efloat', constant) (var float);
      make (elongdouble', constant) (var longdouble);
      make (eint', constant) (var integer);

      make (especial', constant_expression) (var exp3);

      make (u, list_exp2) (var exp2);
      make (u, list_exp2) R.(var exp2 * var comma * var list_exp2);

      make (u, exp) (var exp2);
      make (u, exp2) (var exp3);
      make (u, exp3) (var exp4);
      make (u, exp4) (var exp5);
      make (u, exp5) (var exp6);
      make (u, exp6) (var exp7);
      make (u, exp7) (var exp8);
      make (u, exp8) (var exp9);
      make (u, exp9) (var exp10);
      make (u, exp10) (var exp11);
      make (u, exp11) (var exp12);
      make (u, exp12) (var exp13);
      make (u, exp13) (var exp14);
      make (u, exp14) (var exp15);
      make (u, exp15) (var exp16);
      make (u, exp16) (var exp17);
      make (u, exp17) R.(var lparen * var exp * var rparen);

      make (address', unary_operator) (var address);
      make (indirection', unary_operator) (var indirection);
      make (plus', unary_operator) (var plus_);
      make (negative', unary_operator) (var minus);
      make (complement', unary_operator) (var bitcomp);
      make (logicalneg', unary_operator) (var lneg);

      make (assign', assignment_op) (var assign);
      make (assignmul', assignment_op) (var astassign);
      make (assigndiv', assignment_op) (var slashassign);
      make (assignmod', assignment_op) (var remassign);
      make (assignadd', assignment_op) (var plusassign);
      make (assignsub', assignment_op) (var minusassign);
      make (assignleft', assignment_op) (var lshiftassign);
      make (assignright', assignment_op) (var rshiftassign);
      make (assignand', assignment_op) (var landassign);
      make (assignxor', assignment_op) (var lxorassign);
      make (assignor', assignment_op) (var lorassign);
    ]

  let scanner =
    let letter = R.(range "A" "Z" + range "a" "z" + codes "_") in
    let digit = range "0" "9" in
    let ident_ = R.(letter * star (letter + digit)) in
    let nzdigit = range "1" "9" in
    let hexdigit = R.(range "a" "f" + range "A" "F") in
    let octdigit = range "0" "7" in
    let integer_ = R.(nzdigit * star digit) in
    let hexdecimal_ = R.(codes "0" * codes "xX" * plus (digit + hexdigit)) in
    let octal_ = R.(codes "0" * star octdigit) in
    let double_ = R.(((star digit * codes "." * plus digit) + (plus digit * codes ".")) * (opt (codes "eE" * opt (codes "+-") * plus digit)) + plus digit * codes "eE" * opt (codes "+-") * plus digit) in
    Production.[
      make (u, ident) ident_;
      make (u, string) R.(codes "\"" * star (not_codes "\"\\" + codes "\\" * any) * codes "\"");
      make (u, char) R.(codes "'" * star (not_codes "'\\" + codes "\\" * any) * codes "'");
      make (u, integer) integer_;
      make (u, unsigned) R.(integer_ * codes "uU");
      make (u, long) R.(integer_ * codes "lL");
      make (u, unsignedlong) R.(integer_ * (text "ul" * text "UL"));
      make (u, hexadecimal) hexdecimal_;
      make (u, hexunsigned) R.(hexdecimal_ * codes "uU");
      make (u, hexlong) R.(hexdecimal_ * codes "lL");
      make (u, hexunslong) R.(hexdecimal_ * (text "ul" * text "UL"));
      make (u, octal) octal_;
      make (u, octalunsigned) R.(octal_ * codes "uU");
      make (u, octallong) R.(octal_ * codes "lL");
      make (u, octalunslong) R.(octal_ * (text "ul" * text "UL"));
      make (u, double) double_;
      make (u, float) R.(double_ * codes "fF");
      make (u, longdouble) R.(double_ * codes "lL");

      make (u, type_void) (text "void");
      make (u, type_char) (text "char");
      make (u, type_short) (text "short");
      make (u, type_int) (text "int");
      make (u, type_long) (text "long");
      make (u, type_float) (text "float");
      make (u, type_double) (text "double");
      make (u, type_signed) (text "signed");
      make (u, type_unsigned) (text "unsigned");
      make (u, type_typedef) ident_;

      make (u, typedef) (text "typedef");
      make (u, extern) (text "extern");
      make (u, static) (text "static");
      make (u, auto) (text "auto");
      make (u, register) (text "register");
      make (u, const) (text "const");
      make (u, volatile) (text "volatile");
      make (u, struct_) (text "struct");
      make (u, union) (text "union");
      make (u, enum) (text "enum");
      make (u, case) (text "case");
      make (u, default) (text "default");
      make (u, if_) (text "if");
      make (u, else_) (text "else");
      make (u, switch) (text "switch");
      make (u, while_) (text "while");
      make (u, do_) (text "do");
      make (u, for_) (text "for");
      make (u, goto) (text "goto");
      make (u, continue) (text "continue");
      make (u, break) (text "break");
      make (u, return) (text "return");
      make (u, sizeof) (text "sizeof");

      make (u, ternary_if) (codes "?");
      make (u, ternary_else) (codes ":");
      make (u, address) (codes "&");
      make (u, indirection) (codes "*");
      make (u, bitcomp) (codes "~");
      make (u, lneg) (codes "!");

      make (u, semi) (text ";");
      make (u, lparen) (text "(");
      make (u, rparen) (text ")");
      make (u, comma) (text ",");
      make (u, lbrace) (text "{");
      make (u, rbrace) (text "}");
      make (u, colon) (text ":");
      make (u, lbrak) (text "[");
      make (u, rbrak) (text "]");
      make (u, assign) (text "=");
      make (u, plus_) (text "+");
      make (u, minus) (text "-");
      make (u, ellip) (text "...");
      make (u, bor) (text "|");
      make (u, band) (text "&");
      make (u, lor_) (text "||");
      make (u, land_) (text "&&");
      make (u, bxor) (text "^");
      make (u, eq) (text "==");
      make (u, noteq) (text "!=");
      make (u, lt) (text "<");
      make (u, gt) (text ">");
      make (u, gte) (text ">=");
      make (u, lte) (text "<=");
      make (u, lshift) (text "<<");
      make (u, rshift) (text ">>");
      make (u, ast) (text "*");
      make (u, slash) (text "/");
      make (u, rem) (text "%");
      make (u, plusplus) (text "++");
      make (u, minusminus) (text "--");
      make (u, dot) (text ".");
      make (u, arrow) (text "->");
      make (u, astassign) (text "*=");
      make (u, slashassign) (text "/=");
      make (u, remassign) (text "%=");
      make (u, plusassign) (text "+=");
      make (u, minusassign) (text "-=");
      make (u, lshiftassign) (text "<<=");
      make (u, rshiftassign) (text ">>=");
      make (u, landassign) (text "&=");
      make (u, lxorassign) (text "^=");
      make (u, lorassign) (text "|=");

      make (u, ws) R.(star (codes " \n\r\t"));
    ]

end)

module B = Benchmark.Make(X)

let _ =
  (*let d = X.driver (X.tables ()) in
  X.Run.file (fun c -> d#read c) "test.c";
  Fmt.pr "@[%a@]" Trace.pp d#trace*)
  (*Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot);
  Fmt.pr "@[%s@]" (Dot.string_of_graph (T.Node_packed_forest.to_dot d#forest))*)

  B.benchmark ()
