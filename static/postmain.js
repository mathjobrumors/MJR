
var iframe = document.createElement('iframe');

iframe.onload = function() { 

    if(iframe.contentWindow.document.getElementById('ok')){
        iframe.contentWindow.document.getElementById('ok').outerHTML = ''; 
        window.location.href = window.location.href; 
    }
    
    if(iframe.contentWindow.document.getElementById('error')){
        iframe.contentWindow.document.getElementById('error').outerHTML = ''; 
        document.getElementById('img').src = '/captcha/' + Math.random().toString(16).substr(2, 8);
    }

    if(iframe.contentWindow.document.getElementById('toofast')){
        iframe.contentWindow.document.getElementById('toofast').outerHTML = ''; 
    }

}

iframe.name = "dummyframe";
iframe.style.display = "none";

document.body.appendChild(iframe); 

window.MathJax = {
    tex: {
        inlineMath: [['$', '$']],
        displayMath: [['$$', '$$']]
     }
};

function typeset(code){
    MathJax.startup.promise = 
        MathJax.startup.promise.then(() => {
            MathJax.typesetPromise(code);
        }).catch(
                (err) => console.log('Typeset failed: ' + err.message()));
}

function escape(htmlStr) {
    return htmlStr.replace(/&/g, "&amp;")
          .replace(/</g, "&lt;")
          .replace(/>/g, "&gt;")
          .replace(/"/g, "&quot;")
          .replace(/'/g, "&#39;");        
} 

function recenter(){
    var rect = document.getElementById('img').getBoundingClientRect();
    window.scrollTo(0, window.scrollY + rect.top - window.innerHeight);
}

const topic = document.querySelector('#topicfield');
topic.addEventListener('input', function() {
    // Escape all html to be realistic
    document.getElementById('previewbox').style = 'display:block';
    document.getElementById('titlearea').innerHTML = escape(document.getElementById('topicfield').value);
    if(document.getElementById('topicfield').value === '' && document.getElementById('msgfield').value === ''){
        document.getElementById('previewbox').style = 'display:none';
    }
    if(document.getElementById('topicfield').value === ''){
        document.getElementById('titlearea').innerHTML = '<div style="opacity:0.0">N</div>';
    } 
    if(document.getElementById('msgarea').value === ''){
        document.getElementById('previewarea').innerHTML = '<div style="opacity:0.0">N</div>';
    }    
//    recenter();
}, false);

const txt = document.querySelector('#msgfield');
txt.addEventListener('input', function() {
    document.getElementById('previewbox').style = 'display:block';
    document.getElementById('previewarea').innerHTML = marked.parse(document.getElementById('msgfield').value);
    if(document.getElementById('topicfield').value === '' && document.getElementById('msgfield').value === ''){
        document.getElementById('previewbox').style = 'display:none';
    }
    if(document.getElementById('topicfield').value === ''){
        document.getElementById('titlearea').innerHTML = '<div style="opacity:0.0">N</div>';
    } 
    if(document.getElementById('msgarea').value === ''){
        document.getElementById('previewarea').innerHTML = '<div style="opacity:0.0">N</div>';
    }    
    typeset(['#previewarea']);
//    recenter();
}, false);

window.addEventListener('load', function() {
 
    marked.setOptions({
        gfm: true
    });

    const renderer = {
        heading(text, level) {
        return `
                <h${level + 1}>
                    ${text}
                </h${level + 1}>`;
        },

        html(h){
            return '<span class=\"color:#F66\">[html not allowed]</span>';
        },  

        link(a,b,c){
            if(allowed.some(x => c.includes(x)) && c.includes('http')){
                return c;
            }else{
                return '<span class=\"color:#F66\">[link not allowed]</span>'
            }
        },

        code(a,b,c){
            return `<p><code>${a}</code></p>`
        },

        image(a,b,c){
            return '<span class=\"color:#F66\">[images not allowed]</span>';
        },
    };
    
    marked.use({ renderer });

});
