var barlangEditor = ace.edit("barlangSource");
barlangEditor.setTheme("ace/theme/monokai");
barlangEditor.getSession().setMode("ace/mode/plain_text");
barlangEditor.getSession().on('change', function(e) {
    try {
        window.compileBarlang(
            barlangEditor.getValue(), 
            function(bash) {
                $("#errors").html("Compiled");
                bashEditor.setValue(bash);
            },
            function(error) {
                $("#errors").html(ansi_up.ansi_to_html(error));
            });
    }
    catch(error) {
        $("#errors").html(ansi_up.ansi_to_html(error));
    }
});
barlangEditor.resize();

var bashEditor = ace.edit("bashSource");
bashEditor.setReadOnly(true);
bashEditor.setTheme("ace/theme/monokai");
bashEditor.getSession().setMode("ace/mode/sh");
bashEditor.resize();
