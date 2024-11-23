$(document).on("shiny:connected",
               //function()

                 //{             Shiny.setInputValue("rand", Math.random());
                   //})
// Target all clicks on any element
document.addEventListener('click',(e) =>
                            {
                              // Get element class(es)
                              let elementClass = e.target.className;
                              // If element has class(es)
                              if (elementClass !== '') {
                                Shiny.setInputValue("rand1", elementClass);
                                
                                //alert(elementClass);
                              }
                              // If element has no classes
                              else {
                                console.log('An element without a class was clicked');
                              }
                            }
));


// Create event listener for link clicks with a custom attribute
//document.querySelectorAll('a[data-type="link-outbound"]').forEach(occurence => {
  //occurence.addEventListener('click', (e) => {
    //console.log('An outbound link was clicked');
  //});
//});

