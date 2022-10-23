function report(addr) {                 
    const data = new URLSearchParams();
    data.append(token_name, token)

    fetch(addr, {
        method: 'POST', 
        credentials: 'same-origin', 
        referrerPolicy: 'strict-origin', 
        headers: {
            'Content-Type': 'application/x-www-form-urlencoded',
        },
        body: data,
    }).then((response) => {
        if(response.status == 200){
            alert("Thank you for your report.");
        }
    });
}
