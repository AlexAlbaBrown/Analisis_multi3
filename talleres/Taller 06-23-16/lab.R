<!DOCTYPE html>
<html  dir="ltr" lang="es" xml:lang="es">
<head>
    <title>Analisis Multivariado I - 2016: Laboratorio Componentes Principales</title>
    <link rel="shortcut icon" href="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/theme/1466532404/favicon" />
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<meta name="keywords" content="moodle, Analisis Multivariado I - 2016: Laboratorio Componentes Principales" />
<link rel="stylesheet" type="text/css" href="http://eva.ccee.edu.uy/theme/yui_combo.php?3.7.3/build/cssreset/reset-min.css&amp;3.7.3/build/cssfonts/fonts-min.css&amp;3.7.3/build/cssgrids/grids-min.css&amp;3.7.3/build/cssbase/base-min.css" /><script type="text/javascript" src="http://eva.ccee.edu.uy/theme/yui_combo.php?3.7.3/build/simpleyui/simpleyui-min.js&amp;3.7.3/build/loader/loader-min.js"></script><script id="firstthemesheet" type="text/css">/** Required in order to fix style inclusion problems in IE with YUI **/</script><link rel="stylesheet" type="text/css" href="http://eva.ccee.edu.uy/theme/styles.php/urdata_socialartistica/1466532404/all" />
<script type="text/javascript">
//<![CDATA[
var M = {}; M.yui = {};
M.pageloadstarttime = new Date();
M.cfg = {"wwwroot":"http:\/\/eva.ccee.edu.uy","sesskey":"AFtDREaIWg","loadingicon":"http:\/\/eva.ccee.edu.uy\/theme\/image.php\/urdata_socialartistica\/core\/1466532404\/i\/loading_small","themerev":"1466532404","slasharguments":1,"theme":"urdata_socialartistica","jsrev":"1466532403","svgicons":true};
var moodleConfigFn = function(me) {var p = me.path, b = me.name.replace(/^moodle-/,'').split('-', 3), n = b.pop();if (/(skin|core)/.test(n)) {n = b.pop();me.type = 'css';};me.path = b.join('-')+'/'+n+'/'+n+'.'+me.type;};
var galleryConfigFn = function(me) {var p = me.path,v=M.yui.galleryversion,f;if(/-(skin|core)/.test(me.name)) {me.type = 'css';p = p.replace(/-(skin|core)/, '').replace(/\.js/, '.css').split('/'), f = p.pop().replace(/(\-(min|debug))/, '');if (/-skin/.test(me.name)) {p.splice(p.length,0,v,'assets','skins','sam', f);} else {p.splice(p.length,0,v,'assets', f);};} else {p = p.split('/'), f = p.pop();p.splice(p.length,0,v, f);};me.path = p.join('/');};
var yui2in3ConfigFn = function(me) {if(/-skin|reset|fonts|grids|base/.test(me.name)){me.type='css';me.path=me.path.replace(/\.js/,'.css');me.path=me.path.replace(/\/yui2-skin/,'/assets/skins/sam/yui2-skin');}};
YUI_config = {"base":"http:\/\/eva.ccee.edu.uy\/lib\/yuilib\/3.7.3\/build\/","comboBase":"http:\/\/eva.ccee.edu.uy\/theme\/yui_combo.php?","combine":true,"filter":"","insertBefore":"firstthemesheet","modules":{"core_filepicker":{"name":"core_filepicker","fullpath":"http:\/\/eva.ccee.edu.uy\/lib\/javascript.php\/1466532403\/repository\/filepicker.js","requires":["base","node","node-event-simulate","json","async-queue","io-base","io-upload-iframe","io-form","yui2-treeview","panel","cookie","datatable","datatable-sort","resize-plugin","dd-plugin","escape","moodle-core_filepicker"]},"core_dock":{"name":"core_dock","fullpath":"http:\/\/eva.ccee.edu.uy\/lib\/javascript.php\/1466532403\/blocks\/dock.js","requires":["base","node","event-custom","event-mouseenter","event-resize"]}},"groups":{"moodle":{"name":"moodle","base":"http:\/\/eva.ccee.edu.uy\/theme\/yui_combo.php?moodle\/1466532403\/","comboBase":"http:\/\/eva.ccee.edu.uy\/theme\/yui_combo.php?","combine":true,"filter":"","ext":false,"root":"moodle\/1466532403\/","patterns":{"moodle-":{"group":"moodle","configFn":moodleConfigFn}}},"local":{"name":"gallery","base":"http:\/\/eva.ccee.edu.uy\/lib\/yui\/gallery\/","comboBase":"http:\/\/eva.ccee.edu.uy\/theme\/yui_combo.php?","combine":true,"filter":"","ext":false,"root":"gallery\/","patterns":{"gallery-":{"group":"gallery","configFn":galleryConfigFn}}},"yui2":{"base":"http:\/\/eva.ccee.edu.uy\/lib\/yuilib\/2in3\/2.9.0\/build\/","comboBase":"http:\/\/eva.ccee.edu.uy\/theme\/yui_combo.php?","combine":true,"ext":false,"root":"2in3\/2.9.0\/build\/","patterns":{"yui2-":{"group":"yui2","configFn":yui2in3ConfigFn}}}}};
M.yui.loader = {modules: {}};

//]]>
</script>
<script type="text/javascript" src="http://eva.ccee.edu.uy/lib/javascript.php/1466532403/lib/javascript-static.js"></script>
<script type="text/javascript" src="http://eva.ccee.edu.uy/theme/javascript.php/urdata_socialartistica/1466532404/head"></script>
</head>

<body id="page-mod-resource-view" class="format-topics  path-mod path-mod-resource safari dir-ltr lang-es yui-skin-sam yui3-skin-sam eva-ccee-edu-uy pagelayout-incourse course-1396 context-174059 cmid-77296 category-48 side-pre-only">
<div class="skiplinks"><a class="skip" href="#maincontent">Saltar a contenido principal</a></div>
<script type="text/javascript">
//<![CDATA[
document.body.className += ' jsenabled';
//]]>
</script>


<div id="page">


<div id="page-header">
        <div id="cab_ur">

<div id="logo">
<img src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/theme/1466532404/logo" alt="" /> 
</div>
<div id="right"></div>
</div>
    </div>

    <div id="subcabezal">
    <div class="headermenu">
        		<div class="logininfo">Usted se ha identificado como <a href="http://eva.ccee.edu.uy/user/profile.php?id=25421" title="Ver perfil">Czarnievicz Daniel</a> (<a href="http://eva.ccee.edu.uy/login/logout.php?sesskey=AFtDREaIWg">Salir</a>)</div><div class="langmenu"><form method="get" action="http://eva.ccee.edu.uy/mod/resource/view.php" id="single_select_f576bc661d650913"><div><input type="hidden" name="id" value="77296" /><label for="single_select576bc661d650914"><span class="accesshide " >Idioma</span></label><select id="single_select576bc661d650914" class="select autosubmit langmenu" name="lang"><option value="en">English (en)</option><option value="es_ar">Español - Argentina (es_ar)</option><option selected="selected" value="es">Español - Internacional (es)</option></select><noscript class="inline"><div><input type="submit" value="Ir" /></div></noscript></div></form></div>	    	</div>
<div id="areas" style="float:right;">
<img src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/theme/1466532404/areas" alt="Áreas de Conocimiento" width="250" height="38" border="0" usemap="#ag"/><map name="ag" id="ag">
<area shape="rect" coords="73,2,106,35" href="http://eva.universidad.edu.uy/course/category.php?id=19" target="_self" alt="&Aacute;rea de Tecnolog&iacute;as y Ciencias de la Naturaleza y el H&aacute;bitat" title="&Aacute;rea de Tecnolog&iacute;as y Ciencias de la Naturaleza y el H&aacute;bitat" />
<area shape="rect" coords="109,2,141,35" href="http://eva.universidad.edu.uy/course/category.php?id=20" target="_self" alt="&Aacute;rea Ciencias de la Salud" title="&Aacute;rea Ciencias de la Salud" />
<area shape="rect" coords="144,2,177,34" href="http://eva.universidad.edu.uy/course/category.php?id=18" target="_self" alt="&Aacute;rea Ciencias Sociales y Art&iacute;stica" title="&Aacute;rea Ciencias Sociales y Art&iacute;stica" />
<area shape="rect" coords="180,2,211,34" href="http://eva.universidad.edu.uy/course/category.php?id=23" target="_self" alt="Interior" title="Interior" />
<area shape="rect" coords="215,2,247,34" href="http://eva.universidad.edu.uy/course/category.php?id=107" target="_self" alt="Espacio Interdiscplinario" title="Espacio Interdiscplinario" />
</map></div>
    </div>


                    <div class="navbar">
                <div class="wrapper clearfix">
                    <div class="breadcrumb"><span class="accesshide">Ruta a la página</span><ul role="navigation"><li><a href="http://eva.ccee.edu.uy/">Página Principal</a></li><li> <span class="accesshide " ><span class="arrow_text">/</span>&nbsp;</span><span class="arrow sep">&#x25BA;</span> <a href="http://eva.ccee.edu.uy/my/">Mis cursos</a></li><li> <span class="accesshide " ><span class="arrow_text">/</span>&nbsp;</span><span class="arrow sep">&#x25BA;</span> <a href="http://eva.ccee.edu.uy/course/category.php?id=13">Licenciatura Estadística</a></li><li> <span class="accesshide " ><span class="arrow_text">/</span>&nbsp;</span><span class="arrow sep">&#x25BA;</span> <a href="http://eva.ccee.edu.uy/course/category.php?id=48">Análisis Multivariado</a></li><li> <span class="accesshide " ><span class="arrow_text">/</span>&nbsp;</span><span class="arrow sep">&#x25BA;</span> <a title="Analisis Multivariado I - 2016" href="http://eva.ccee.edu.uy/course/view.php?id=1396">Analisis Multivariado I - 2016</a></li><li> <span class="accesshide " ><span class="arrow_text">/</span>&nbsp;</span><span class="arrow sep">&#x25BA;</span> <span tabindex="0">Tema 2</span></li><li> <span class="accesshide " ><span class="arrow_text">/</span>&nbsp;</span><span class="arrow sep">&#x25BA;</span> <a title="Archivo" href="http://eva.ccee.edu.uy/mod/resource/view.php?id=77296" id="action_link576bc661d650915">Laboratorio Componentes Principales</a></li></ul></div>
                    <div class="navbutton"> </div>
                </div>
            </div>
        


<!-- END OF HEADER -->
<div id="page-content-wrapper" class="wrapper clearfix">
    <div id="page-content">
        <div id="region-main-box">
            <div id="region-post-box">

                <div id="region-main-wrap">
                    <div id="region-main">
                        <div class="region-content">
                            <div role="main"><span id="maincontent"></span><h2 id="resourceheading" class="main">Laboratorio Componentes Principales</h2><div id="resourceintro" class="box mod_introbox"><div class="no-overflow"><p>Laboratorio Componentes Principales</p></div></div><div class="resourceworkaround">Haga clic en <a href="http://eva.ccee.edu.uy/pluginfile.php/174059/mod_resource/content/1/Laboratorio%20Componentes%20Principales.R" onclick="window.open('http://eva.ccee.edu.uy/pluginfile.php/174059/mod_resource/content/1/Laboratorio%20Componentes%20Principales.R', '', 'width=620,height=450,toolbar=no,location=no,menubar=no,copyhistory=no,status=no,directories=no,scrollbars=yes,resizable=yes'); return false;">Laboratorio Componentes Principales.R</a> para ver el archivo.</div></div>                        </div>
                    </div>
                </div>

                                <div id="region-pre" class="block-region">
                    <div class="region-content">
                        <a href="#sb-1" class="skip-block">Saltar Navegación</a><div id="inst41469" class="block_navigation  block" role="navigation" aria-labelledby="instance-41469-header"><div class="header"><div class="title"><div class="block_action"></div><h2 id="instance-41469-header">Navegación</h2></div></div><div class="content"><ul class="block_tree list"><li class="type_unknown depth_1 contains_branch" aria-expanded="true"><p class="tree_item branch canexpand navigation_node"><a href="http://eva.ccee.edu.uy/">Página Principal</a></p><ul><li class="type_setting depth_2 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/my/"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Área personal</a></p></li>
<li class="type_course depth_2 collapsed contains_branch" aria-expanded="false"><p class="tree_item branch"><span title="Facultad de Ciencias Económicas y de Administración" tabindex="0">Páginas del sitio</span></p><ul><li class="type_custom depth_3 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/blog/index.php?courseid=0"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Blogs del sitio</a></p></li>
<li class="type_custom depth_3 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/tag/search.php"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Marcas</a></p></li>
<li class="type_custom depth_3 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/calendar/view.php?view=month"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Calendario</a></p></li>
<li class="type_activity depth_3 item_with_icon"><p class="tree_item leaf hasicon"><a title="Foro" href="http://eva.ccee.edu.uy/mod/forum/view.php?id=13"><img alt="Foro" class="smallicon navicon" title="Foro" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/forum/1466532404/icon" />Novedades</a></p></li>
<li class="type_activity depth_3 item_with_icon"><p class="tree_item leaf hasicon"><a title="Archivo" href="http://eva.ccee.edu.uy/mod/resource/view.php?id=902"><img alt="Archivo" class="smallicon navicon" title="Archivo" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/f/pdf-24" />Tutorial Estudiantes</a></p></li>
<li class="type_activity depth_3 item_with_icon"><p class="tree_item leaf hasicon"><a title="Página" href="http://eva.ccee.edu.uy/mod/page/view.php?id=3118" id="action_link576bc661d65091"><img alt="Página" class="smallicon navicon" title="Página" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/page/1466532404/icon" />Respuestas a Preguntas Frecuentes</a></p></li>
<li class="type_activity depth_3 item_with_icon"><p class="tree_item leaf hasicon"><a title="Página" href="http://eva.ccee.edu.uy/mod/page/view.php?id=77228"><img alt="Página" class="smallicon navicon" title="Página" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/page/1466532404/icon" />...artamento de Economía (DE) - Información detallada</a></p></li>
<li class="type_activity depth_3 item_with_icon"><p class="tree_item leaf hasicon"><a title="Página" href="http://eva.ccee.edu.uy/mod/page/view.php?id=74095"><img alt="Página" class="smallicon navicon" title="Página" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/page/1466532404/icon" />Solicitud de creación de usuario en el EVA FCEA</a></p></li>
<li class="type_activity depth_3 item_with_icon"><p class="tree_item leaf hasicon"><a title="Página" href="http://eva.ccee.edu.uy/mod/page/view.php?id=74096"><img alt="Página" class="smallicon navicon" title="Página" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/page/1466532404/icon" />Solicitud de creación de curso en el EVA</a></p></li>
<li class="type_activity depth_3 item_with_icon"><p class="tree_item leaf hasicon"><a title="Archivo" href="http://eva.ccee.edu.uy/mod/resource/view.php?id=74681"><img alt="Archivo" class="smallicon navicon" title="Archivo" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/f/pdf-24" />Protocolo de gestión de cursos y usuarios</a></p></li>
<li class="type_activity depth_3 item_with_icon"><p class="tree_item leaf hasicon"><a title="Archivo" href="http://eva.ccee.edu.uy/mod/resource/view.php?id=74680"><img alt="Archivo" class="smallicon navicon" title="Archivo" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/f/pdf-24" />Articuladores EVA FCEA</a></p></li>
<li class="type_activity depth_3 item_with_icon"><p class="tree_item leaf hasicon"><a title="URL" href="http://eva.ccee.edu.uy/mod/url/view.php?id=36975" id="action_link576bc661d65092"><img alt="URL" class="smallicon navicon" title="URL" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/url/1466532404/icon" />Videotutoriales EVA Moodle 2.4</a></p></li></ul></li>
<li class="type_user depth_2 collapsed contains_branch" aria-expanded="false"><p class="tree_item branch"><span tabindex="0">Mi perfil</span></p><ul><li class="type_custom depth_3 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/user/profile.php?id=25421"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Ver perfil</a></p></li>
<li class="type_custom depth_3 collapsed contains_branch" aria-expanded="false"><p class="tree_item branch"><span tabindex="0">Mensajes en foros</span></p><ul><li class="type_custom depth_4 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/mod/forum/user.php?id=25421"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Mensajes</a></p></li>
<li class="type_custom depth_4 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/mod/forum/user.php?id=25421&amp;mode=discussions"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Debates</a></p></li></ul></li>
<li class="type_unknown depth_3 collapsed contains_branch" aria-expanded="false"><p class="tree_item branch"><span tabindex="0">Blogs</span></p><ul><li class="type_custom depth_4 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/blog/index.php?userid=25421"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Ver todas mis entradas</a></p></li>
<li class="type_custom depth_4 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/blog/edit.php?action=add"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Añadir una nueva entrada</a></p></li></ul></li>
<li class="type_setting depth_3 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/message/index.php"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Mensajes</a></p></li>
<li class="type_setting depth_3 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/user/files.php"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Mis archivos privados</a></p></li></ul></li>
<li class="type_system depth_2 contains_branch" aria-expanded="true"><p class="tree_item branch"><span tabindex="0">Curso actual</span></p><ul><li class="type_course depth_3 contains_branch" aria-expanded="true"><p class="tree_item branch canexpand"><a title="Analisis Multivariado I - 2016" href="http://eva.ccee.edu.uy/course/view.php?id=1396">Analisis Multivariado I - 2016</a></p><ul><li class="type_unknown depth_4 collapsed contains_branch" aria-expanded="false"><p class="tree_item branch"><a href="http://eva.ccee.edu.uy/user/index.php?id=1396">Participantes</a></p><ul><li class="type_custom depth_5 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/blog/index.php?courseid=1396"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Blogs de curso</a></p></li>
<li class="type_user depth_5 collapsed contains_branch" aria-expanded="false"><p class="tree_item branch"><a href="http://eva.ccee.edu.uy/user/view.php?id=25421&amp;course=1396">Czarnievicz Daniel</a></p><ul><li class="type_custom depth_6 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/user/view.php?id=25421&amp;course=1396"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Ver perfil</a></p></li>
<li class="type_custom depth_6 collapsed contains_branch" aria-expanded="false"><p class="tree_item branch"><span tabindex="0">Mensajes en foros</span></p><ul><li class="type_custom depth_7 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/mod/forum/user.php?id=25421&amp;course=1396"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Mensajes</a></p></li>
<li class="type_custom depth_7 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/mod/forum/user.php?id=25421&amp;course=1396&amp;mode=discussions"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Debates</a></p></li></ul></li>
<li class="type_unknown depth_6 collapsed contains_branch" aria-expanded="false"><p class="tree_item branch"><span tabindex="0">Blogs</span></p><ul><li class="type_custom depth_7 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/blog/index.php?userid=25421"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Ver todas mis entradas</a></p></li>
<li class="type_custom depth_7 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/blog/edit.php?action=add"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Añadir una nueva entrada</a></p></li></ul></li>
<li class="type_setting depth_6 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/message/index.php"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Mensajes</a></p></li>
<li class="type_setting depth_6 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/user/files.php"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Mis archivos privados</a></p></li>
<li class="type_custom depth_6 collapsed contains_branch" aria-expanded="false"><p class="tree_item branch"><span tabindex="0">Informes de actividad</span></p><ul><li class="type_custom depth_7 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/report/log/user.php?id=25421&amp;course=1396&amp;mode=today"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Registros de hoy</a></p></li>
<li class="type_custom depth_7 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/report/log/user.php?id=25421&amp;course=1396&amp;mode=all"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Todas las entradas</a></p></li>
<li class="type_custom depth_7 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/report/outline/user.php?id=25421&amp;course=1396&amp;mode=outline"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Diagrama de informe</a></p></li>
<li class="type_custom depth_7 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/report/outline/user.php?id=25421&amp;course=1396&amp;mode=complete"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Informe completo</a></p></li>
<li class="type_custom depth_7 item_with_icon"><p class="tree_item leaf hasicon"><a href="http://eva.ccee.edu.uy/report/stats/user.php?id=25421&amp;course=1396"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Estadísticas</a></p></li></ul></li></ul></li></ul></li>
<li class="type_structure depth_4 collapsed contains_branch" aria-expanded="false"><p class="tree_item branch" id="expandable_branch_30_14628"><span tabindex="0">General</span></p></li>
<li class="type_structure depth_4 collapsed contains_branch" aria-expanded="false"><p class="tree_item branch" id="expandable_branch_30_14629"><span tabindex="0">Tema 1</span></p></li>
<li class="type_structure depth_4 contains_branch" aria-expanded="true"><p class="tree_item branch"><span tabindex="0">Tema 2</span></p><ul><li class="type_activity depth_5 item_with_icon"><p class="tree_item leaf hasicon"><a title="Carpeta" href="http://eva.ccee.edu.uy/mod/folder/view.php?id=74124"><img alt="Carpeta" class="smallicon navicon" title="Carpeta" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/folder/1466532404/icon" />Manuales de R</a></p></li>
<li class="type_activity depth_5 item_with_icon"><p class="tree_item leaf hasicon"><a title="Carpeta" href="http://eva.ccee.edu.uy/mod/folder/view.php?id=74127"><img alt="Carpeta" class="smallicon navicon" title="Carpeta" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/folder/1466532404/icon" />ANÁLISIS DE CLUSTER_Transparencias de clase</a></p></li>
<li class="type_activity depth_5 item_with_icon"><p class="tree_item leaf hasicon"><a title="Carpeta" href="http://eva.ccee.edu.uy/mod/folder/view.php?id=74248"><img alt="Carpeta" class="smallicon navicon" title="Carpeta" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/folder/1466532404/icon" />LIBRO JORGE BLANCO</a></p></li>
<li class="type_activity depth_5 item_with_icon"><p class="tree_item leaf hasicon"><a title="Carpeta" href="http://eva.ccee.edu.uy/mod/folder/view.php?id=75173"><img alt="Carpeta" class="smallicon navicon" title="Carpeta" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/folder/1466532404/icon" />MATERIAL DISCRIMINANTE E INFERENCIA MULTIVARIADA</a></p></li>
<li class="type_activity depth_5 item_with_icon"><p class="tree_item leaf hasicon"><a title="Archivo" href="http://eva.ccee.edu.uy/mod/resource/view.php?id=76284" id="action_link576bc661d65093"><img alt="Archivo" class="smallicon navicon" title="Archivo" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/f/pdf-24" />Repaso Algebra Lineal. Matriz de datos</a></p></li>
<li class="type_activity depth_5 item_with_icon"><p class="tree_item leaf hasicon"><a title="Archivo" href="http://eva.ccee.edu.uy/mod/resource/view.php?id=76285" id="action_link576bc661d65094"><img alt="Archivo" class="smallicon navicon" title="Archivo" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/f/pdf-24" />Práctico Repaso de Algebra Lineal</a></p></li>
<li class="type_activity depth_5 item_with_icon"><p class="tree_item leaf hasicon"><a title="Archivo" href="http://eva.ccee.edu.uy/mod/resource/view.php?id=76286" id="action_link576bc661d65095"><img alt="Archivo" class="smallicon navicon" title="Archivo" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/f/pdf-24" />Práctico Matriz de datos</a></p></li>
<li class="type_activity depth_5 item_with_icon"><p class="tree_item leaf hasicon"><a title="Archivo" href="http://eva.ccee.edu.uy/mod/resource/view.php?id=76629" id="action_link576bc661d65096"><img alt="Archivo" class="smallicon navicon" title="Archivo" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/f/pdf-24" />Ejercicios para repasar cosas de R</a></p></li>
<li class="type_activity depth_5 item_with_icon"><p class="tree_item leaf hasicon"><a title="Archivo" href="http://eva.ccee.edu.uy/mod/resource/view.php?id=76814" id="action_link576bc661d65097"><img alt="Archivo" class="smallicon navicon" title="Archivo" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/f/pdf-24" />Componentes Principales I</a></p></li>
<li class="type_activity depth_5 item_with_icon"><p class="tree_item leaf hasicon"><a title="Archivo" href="http://eva.ccee.edu.uy/mod/resource/view.php?id=76815" id="action_link576bc661d65098"><img alt="Archivo" class="smallicon navicon" title="Archivo" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/f/pdf-24" />Ejemplo ACP (Carmona)</a></p></li>
<li class="type_activity depth_5 item_with_icon"><p class="tree_item leaf hasicon"><a title="Archivo" href="http://eva.ccee.edu.uy/mod/resource/view.php?id=77089" id="action_link576bc661d65099"><img alt="Archivo" class="smallicon navicon" title="Archivo" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/f/pdf-24" />Analisis Factorial - Componentes Principales</a></p></li>
<li class="type_activity depth_5 item_with_icon"><p class="tree_item leaf hasicon"><a title="Archivo" href="http://eva.ccee.edu.uy/mod/resource/view.php?id=77090" id="action_link576bc661d650910"><img alt="Archivo" class="smallicon navicon" title="Archivo" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/f/pdf-24" />Práctico 3 Componentes Principales</a></p></li>
<li class="type_activity depth_5 item_with_icon"><p class="tree_item leaf hasicon"><a title="Archivo" href="http://eva.ccee.edu.uy/mod/resource/view.php?id=77297" id="action_link576bc661d650911"><img alt="Archivo" class="smallicon navicon" title="Archivo" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/f/text-24" />w2000.txt</a></p></li>
<li class="type_activity depth_5 item_with_icon current_branch"><p class="tree_item leaf hasicon active_tree_node"><a title="Archivo" href="http://eva.ccee.edu.uy/mod/resource/view.php?id=77296" id="action_link576bc661d650912"><img alt="Archivo" class="smallicon navicon" title="Archivo" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/f/text-24" />Laboratorio Componentes Principales</a></p></li></ul></li>
<li class="type_structure depth_4 collapsed contains_branch" aria-expanded="false"><p class="tree_item branch" id="expandable_branch_30_14631"><span tabindex="0">Tema 3</span></p></li>
<li class="type_structure depth_4 collapsed contains_branch" aria-expanded="false"><p class="tree_item branch" id="expandable_branch_30_14633"><span tabindex="0">Tema 4</span></p></li>
<li class="type_structure depth_4 collapsed contains_branch" aria-expanded="false"><p class="tree_item branch" id="expandable_branch_30_14635"><span tabindex="0">Tema 5</span></p></li>
<li class="type_structure depth_4 collapsed contains_branch" aria-expanded="false"><p class="tree_item branch" id="expandable_branch_30_14636"><span tabindex="0">Tema 6</span></p></li>
<li class="type_structure depth_4 collapsed contains_branch" aria-expanded="false"><p class="tree_item branch" id="expandable_branch_30_14634"><span tabindex="0">Tema 7</span></p></li></ul></li></ul></li>
<li class="type_system depth_2 collapsed contains_branch" aria-expanded="false"><p class="tree_item branch" id="expandable_branch_0_mycourses"><a href="http://eva.ccee.edu.uy/my/">Mis cursos</a></p></li></ul></li></ul></div></div><span id="sb-1" class="skip-block-to"></span><a href="#sb-2" class="skip-block">Saltar Ajustes</a><div id="inst41470" class="block_settings  block" role="navigation" aria-labelledby="instance-41470-header"><div class="header"><div class="title"><div class="block_action"></div><h2 id="instance-41470-header">Ajustes</h2></div></div><div class="content"><div id="settingsnav" class="box block_tree_box"><ul class="block_tree list"><li class="type_course collapsed contains_branch" aria-expanded="false"><p class="tree_item branch root_node"><span tabindex="0">Administración del curso</span></p><ul><li class="type_setting collapsed item_with_icon"><p class="tree_item leaf"><a href="http://eva.ccee.edu.uy/enrol/self/unenrolself.php?enrolid=3872"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/user" />Darme de baja en Analisis Multivariado I - 2016</a></p></li></ul></li>
<li class="type_unknown collapsed contains_branch" aria-expanded="false"><hr /><p class="tree_item branch root_node" id="usersettings"><span tabindex="0">Ajustes de mi perfil</span></p><ul><li class="type_setting collapsed item_with_icon"><p class="tree_item leaf"><a href="http://eva.ccee.edu.uy/user/edit.php?id=25421&amp;course=1396"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Editar perfil</a></p></li>
<li class="type_setting collapsed item_with_icon"><p class="tree_item leaf"><a href="http://eva.ccee.edu.uy/message/edit.php?id=25421"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Mensajería</a></p></li>
<li class="type_unknown collapsed contains_branch" aria-expanded="false"><p class="tree_item branch"><span tabindex="0">Blogs</span></p><ul><li class="type_setting collapsed item_with_icon"><p class="tree_item leaf"><a href="http://eva.ccee.edu.uy/blog/preferences.php"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Preferencias</a></p></li>
<li class="type_setting collapsed item_with_icon"><p class="tree_item leaf"><a href="http://eva.ccee.edu.uy/blog/external_blogs.php"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Blogs externos</a></p></li>
<li class="type_setting collapsed item_with_icon"><p class="tree_item leaf"><a href="http://eva.ccee.edu.uy/blog/external_blog_edit.php"><img alt="" class="smallicon navicon" title="" src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/core/1466532404/i/navigationitem" />Registrar un blog externo</a></p></li></ul></li></ul></li></ul></div></div></div><span id="sb-2" class="skip-block-to"></span>                    </div>
                </div>
                
                
            </div>
        </div>
    </div>
</div>
<div align="center">


<div id="infoPie"> 

<small>
<div class="logininfo">Usted se ha identificado como <a href="http://eva.ccee.edu.uy/user/profile.php?id=25421" title="Ver perfil">Czarnievicz Daniel</a> (<a href="http://eva.ccee.edu.uy/login/logout.php?sesskey=AFtDREaIWg">Salir</a>)</div>		<br/> <br/>


        <div class="homelink"><a href="http://eva.ccee.edu.uy/course/view.php?id=1396">Analisis Multivariado I - 2016</a></div></small>

<br/>
<p style="font-weight: bold; color:#6d6e71;"> Por consultas o asistencia contactarse con <a href="http://data.cse.edu.uy/encargados-eva-por-servicio" target="_blank"> el o la encargado/a del EVA </a> en su Servicio Universitario </p>
<br/> 
<p style="color:#6d6e71"> Programa para el Desarrollo de Entornos Virtuales de Aprendizaje en la Universidad de la Rep&uacute;blica (ProEVA) - Departamento de Apoyo T&eacute;cnico Acad&eacute;mico (DATA) | <a href="http://data.cse.edu.uy/data_consultas" target="_blank"> contacto </a> </p>

</div></div>



<!-- START OF FOOTER -->




    <div id="page-footer" class="wrapper">
       
    </div>


<div id="pie_ur"><div style="float:left">

<img src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/theme/1466532404/left_pie"  alt="Universidad de la Rep&uacute;blica"/>

</div><div style="float:right">

<img src="http://eva.ccee.edu.uy/theme/image.php/urdata_socialartistica/theme/1466532404/right_pie"  alt="Universidad de la Rep&uacute;blica"/></div></div>
 

<!-- END OF FOOTER -->

</div><p>&nbsp;</p>
<script type="text/javascript" src="http://eva.ccee.edu.uy/theme/javascript.php/urdata_socialartistica/1466532404/footer"></script>
<script type="text/javascript">
//<![CDATA[
M.str = {"moodle":{"lastmodified":"\u00daltima modificaci\u00f3n","name":"Nombre","error":"Error","info":"Informaci\u00f3n","viewallcourses":"Ver todos los cursos","cancel":"Cancelar","yes":"S\u00ed"},"repository":{"type":"Tipo","size":"Tama\u00f1o","invalidjson":"Cadena JSON no v\u00e1lida","nofilesattached":"No se han adjuntado archivos","filepicker":"Selector de archivos","logout":"Salir","nofilesavailable":"No hay archivos disponibles","norepositoriesavailable":"Lo sentimos, ninguno de sus repositorios actuales puede devolver archivos en el formato solicitado.","fileexistsdialogheader":"El archivo existe","fileexistsdialog_editor":"Un archivo con el mismo nombre ya se ha adjuntado al texto que est\u00e1 editando.","fileexistsdialog_filemanager":"Un archivo con ese nombre ya ha sido adjuntado","renameto":"Cambiar el nombre a","referencesexist":"Existen {$a} archivos de alias\/atajos que emplean este archivo como su or\u00edgen"},"block":{"addtodock":"Minimizar en la barra lateral","undockitem":"Desacoplar este \u00edtem","undockall":"Desacoplar todo","hidedockpanel":"Esconder el panel desacoplado","hidepanel":"Esconder panel"},"langconfig":{"thisdirectionvertical":"btt"},"admin":{"confirmation":"Confirmaci\u00f3n"}};
//]]>
</script>
<script type="text/javascript">
//<![CDATA[
var navtreeexpansions41469 = [{"id":"expandable_branch_30_14628","key":"14628","type":30},{"id":"expandable_branch_30_14629","key":"14629","type":30},{"id":"expandable_branch_30_14631","key":"14631","type":30},{"id":"expandable_branch_30_14633","key":"14633","type":30},{"id":"expandable_branch_30_14635","key":"14635","type":30},{"id":"expandable_branch_30_14636","key":"14636","type":30},{"id":"expandable_branch_30_14634","key":"14634","type":30},{"id":"expandable_branch_0_mycourses","key":"mycourses","type":0}];
//]]>
</script>
<script type="text/javascript">
//<![CDATA[
YUI().use('node', function(Y) {
M.util.load_flowplayer();
setTimeout("fix_column_widths()", 20);
function legacy_activity_onclick_handler_1(e) { e.halt(); window.open('http://eva.ccee.edu.uy/mod/page/view.php?id=3118&inpopup=1', '', 'width=620,height=450,toolbar=no,location=no,menubar=no,copyhistory=no,status=no,directories=no,scrollbars=yes,resizable=yes'); return false; };
function legacy_activity_onclick_handler_2(e) { e.halt(); window.open('http://eva.ccee.edu.uy/mod/url/view.php?id=36975&redirect=1', '', 'width=620,height=450,toolbar=no,location=no,menubar=no,copyhistory=no,status=no,directories=no,scrollbars=yes,resizable=yes'); return false; };
function legacy_activity_onclick_handler_3(e) { e.halt(); window.open('http://eva.ccee.edu.uy/mod/resource/view.php?id=76284&redirect=1', '', 'width=620,height=450,toolbar=no,location=no,menubar=no,copyhistory=no,status=no,directories=no,scrollbars=yes,resizable=yes'); return false; };
function legacy_activity_onclick_handler_4(e) { e.halt(); window.open('http://eva.ccee.edu.uy/mod/resource/view.php?id=76285&redirect=1', '', 'width=620,height=450,toolbar=no,location=no,menubar=no,copyhistory=no,status=no,directories=no,scrollbars=yes,resizable=yes'); return false; };
function legacy_activity_onclick_handler_5(e) { e.halt(); window.open('http://eva.ccee.edu.uy/mod/resource/view.php?id=76286&redirect=1', '', 'width=620,height=450,toolbar=no,location=no,menubar=no,copyhistory=no,status=no,directories=no,scrollbars=yes,resizable=yes'); return false; };
function legacy_activity_onclick_handler_6(e) { e.halt(); window.open('http://eva.ccee.edu.uy/mod/resource/view.php?id=76629&redirect=1', '', 'width=620,height=450,toolbar=no,location=no,menubar=no,copyhistory=no,status=no,directories=no,scrollbars=yes,resizable=yes'); return false; };
function legacy_activity_onclick_handler_7(e) { e.halt(); window.open('http://eva.ccee.edu.uy/mod/resource/view.php?id=76814&redirect=1', '', 'width=620,height=450,toolbar=no,location=no,menubar=no,copyhistory=no,status=no,directories=no,scrollbars=yes,resizable=yes'); return false; };
function legacy_activity_onclick_handler_8(e) { e.halt(); window.open('http://eva.ccee.edu.uy/mod/resource/view.php?id=76815&redirect=1', '', 'width=620,height=450,toolbar=no,location=no,menubar=no,copyhistory=no,status=no,directories=no,scrollbars=yes,resizable=yes'); return false; };
function legacy_activity_onclick_handler_9(e) { e.halt(); window.open('http://eva.ccee.edu.uy/mod/resource/view.php?id=77089&redirect=1', '', 'width=620,height=450,toolbar=no,location=no,menubar=no,copyhistory=no,status=no,directories=no,scrollbars=yes,resizable=yes'); return false; };
function legacy_activity_onclick_handler_10(e) { e.halt(); window.open('http://eva.ccee.edu.uy/mod/resource/view.php?id=77090&redirect=1', '', 'width=620,height=450,toolbar=no,location=no,menubar=no,copyhistory=no,status=no,directories=no,scrollbars=yes,resizable=yes'); return false; };
function legacy_activity_onclick_handler_11(e) { e.halt(); window.open('http://eva.ccee.edu.uy/mod/resource/view.php?id=77297&redirect=1', '', 'width=620,height=450,toolbar=no,location=no,menubar=no,copyhistory=no,status=no,directories=no,scrollbars=yes,resizable=yes'); return false; };
function legacy_activity_onclick_handler_12(e) { e.halt(); window.open('http://eva.ccee.edu.uy/mod/resource/view.php?id=77296&redirect=1', '', 'width=620,height=450,toolbar=no,location=no,menubar=no,copyhistory=no,status=no,directories=no,scrollbars=yes,resizable=yes'); return false; };
M.yui.galleryversion="2010.04.08-12-35";Y.use("core_dock","moodle-block_navigation-navigation",function() {M.block_navigation.init_add_tree({"id":"41469","instance":"41469","candock":true,"courselimit":"20","expansionlimit":0});
});
M.yui.galleryversion="2010.04.08-12-35";Y.use("core_dock","moodle-block_navigation-navigation",function() {M.block_navigation.init_add_tree({"id":"41470","instance":"41470","candock":true});
});
M.util.help_popups.setup(Y);
M.yui.galleryversion="2010.04.08-12-35";Y.use("moodle-core-formautosubmit",function() {M.core.init_formautosubmit({"selectid":"single_select576bc661d650914","nothing":false});
});
M.util.init_block_hider(Y, {"id":"inst41469","title":"Navegaci\u00f3n","preference":"block41469hidden","tooltipVisible":"Ocultar bloque Navegaci\u00f3n","tooltipHidden":"Mostrar bloque Navegaci\u00f3n"});
M.util.init_block_hider(Y, {"id":"inst41470","title":"Ajustes","preference":"block41470hidden","tooltipVisible":"Ocultar bloque Ajustes","tooltipHidden":"Mostrar bloque Ajustes"});
Y.on('click', legacy_activity_onclick_handler_1, "#action_link576bc661d65091", null);
Y.on('click', legacy_activity_onclick_handler_2, "#action_link576bc661d65092", null);
Y.on('click', legacy_activity_onclick_handler_3, "#action_link576bc661d65093", null);
Y.on('click', legacy_activity_onclick_handler_4, "#action_link576bc661d65094", null);
Y.on('click', legacy_activity_onclick_handler_5, "#action_link576bc661d65095", null);
Y.on('click', legacy_activity_onclick_handler_6, "#action_link576bc661d65096", null);
Y.on('click', legacy_activity_onclick_handler_7, "#action_link576bc661d65097", null);
Y.on('click', legacy_activity_onclick_handler_8, "#action_link576bc661d65098", null);
Y.on('click', legacy_activity_onclick_handler_9, "#action_link576bc661d65099", null);
Y.on('click', legacy_activity_onclick_handler_10, "#action_link576bc661d650910", null);
Y.on('click', legacy_activity_onclick_handler_11, "#action_link576bc661d650911", null);
Y.on('click', legacy_activity_onclick_handler_12, "#action_link576bc661d650912", null);
Y.on('click', legacy_activity_onclick_handler_12, "#action_link576bc661d650915", null);

});
//]]>
</script>
</body>
</html>