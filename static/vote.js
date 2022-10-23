var iframe = document.createElement('iframe');

iframe.onload = function() { 

    if(iframe.contentWindow.document.getElementById('ok')){
        window.document.location.assign(url);
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

function quote(div){
        const textarea = document.getElementById('field');

        textarea.focus();

        textarea.value += '\n';
        textarea.value += document.getElementById(div).getAttribute('data-value');
        textarea.value += '\n';

        textarea.selectionStart = textarea.value.length;
    
        document.getElementById('previewarea').innerHTML = marked.parse(document.getElementById('field').value);
        document.getElementById('trebuchet').style = "display:block";
        typeset(['#previewarea']);

}

function vote(addr, upButton, initIsUp, initIsDown, initNumUp, initNumDown, upId, downId) {
    const data = new URLSearchParams();
    data.append(token_name, token)

    var curIsUp = document.getElementById(upId).dataset.voted;
    var curIsDown = document.getElementById(downId).dataset.voted;
    
    var r = casework(upButton, curIsUp, curIsDown, initIsUp, initIsDown, initNumUp, initNumDown);
    
    if (r.nextIsUp){
        document.getElementById(upId).innerHTML = "<b>" + r.nextUpNum.toString() + "</b>";    
    }else{
        document.getElementById(upId).innerHTML = r.nextUpNum.toString();
    }
    
    if(r.nextIsDown){
        document.getElementById(downId).innerHTML = "<b>" + r.nextDownNum.toString() + "</b>";    
    }else{
        document.getElementById(downId).innerHTML = r.nextDownNum.toString();
    }
    
    document.getElementById(downId).dataset.voted = r.nextIsDown.toString();
    document.getElementById(upId).dataset.voted = r.nextIsUp.toString();

    fetch(addr, {
            method: 'POST', 
            credentials: 'same-origin', 
            referrerPolicy: 'strict-origin', 
            headers: {
                'Content-Type': 'application/x-www-form-urlencoded',
            },
            body: data,
        }).then((response) => {              
            if(response.status != 200){
                if(response.status == 400){
                            alert("you click too fast");
                }else{
                            alert("Server failed to post vote");
                }
            }
            });

}

function report(addr) {
    
    const data = new URLSearchParams();
    data.append(token_name, token)

    alert("Thank you for your report.");
    
    fetch(addr, {
        method: 'POST', 
        credentials: 'same-origin', 
        referrerPolicy: 'strict-origin', 
        headers: {
            'Content-Type': 'application/x-www-form-urlencoded',
        },
        body: data,
    });
}
       
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

function recenter(){
    var rect = document.getElementById('img').getBoundingClientRect();
    window.scrollTo(0, window.scrollY + rect.top - window.innerHeight);
}

const txt = document.querySelector('#field');
txt.addEventListener('input', function() {
    document.getElementById('previewarea').innerHTML = marked.parse(document.getElementById('field').value);
    document.getElementById('trebuchet').style = "display:block";
    if(document.getElementById('field').value === ''){
        document.getElementById('previewarea').innerHTML = '<div style=\"opacity:0.0\">N</div>';
        document.getElementById('trebuchet').style = "display:none";
    }
    typeset(['#previewarea']);
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
        }
    };
  
    marked.use({ renderer });

});

