
function casework(upButton, curIsUp, curIsDown, initIsUp, initIsDown, initNumUp, initNumDown){
                
    var r = { nextIsUp : 0, nextIsDown : 0, nextUpNum : 0, nextDownNum : 0 };
    
    if(upButton == 1){

	if(curIsUp == 0){

	    r.nextIsUp = 1; 
        r.nextIsDown = 0;
    
        if(initIsUp == 0)
            r.nextUpNum = initNumUp + 1;

        if(initIsUp == 1)
            r.nextUpNum = initNumUp;
	    
        if(initIsDown == 1)
            r.nextDownNum = initNumDown - 1;
    
        if(initIsDown == 0)
            r.nextDownNum = initNumDown;

        }
	
    }
    else
    {
	
        if(curIsDown == 0){
          
            r.nextIsDown = 1;
            r.nextIsUp = 0;
          
            if(initIsDown == 0)
                r.nextDownNum = initNumDown + 1;
          
            if(initIsDown == 1)
                r.nextDownNum = initNumDown; 

            if(initIsUp == 1)
                r.nextUpNum = initNumUp - 1;
            
            if(initIsUp == 0)
                r.nextUpNum = initNumUp;
        
        }
    }

    if(upButton == 1){
    
        if(curIsUp == 1 && curIsDown == 0){
    
            r.nextIsUp = 0;
            r.nextIsDown = 0;
    
            if(initIsUp == 0)
                r.nextUpNum = initNumUp;
    
            if(initIsUp == 1)
                r.nextUpNum = initNumUp - 1;

            if(initIsDown == 1)
                r.nextDownNum = initNumDown - 1;

            if(initIsDown == 0)
                r.nextDownNum = initNumDown;

        }
    }
    else
    {

        if(curIsDown == 1 && curIsUp == 0){

            nextIsDown = 0;
            nextIsUp = 0;

            if(initIsDown == 0)
                r.nextDownNum = initNumDown;

            if(initIsDown == 1)
                r.nextDownNum = initNumDown - 1;

            if(initIsUp == 1)
                r.nextUpNum = initNumUp - 1;
    
            if(initIsUp == 0)
                r.nextUpNum = initNumUp;

        }
    }

    return r;

}
