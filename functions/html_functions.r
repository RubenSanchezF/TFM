
#   NAME: footerBox
#   DESCRIPTION: Creates a footer, this function should be inside the ui 
#   ARGUMENTS:
#     text: Bold text to display on the left side of the pannel
#     company: Name of your company in a different color
#     text2: Text to display after the previous fields
#     text: Bold text to display on the right side of the pannel
footerBox <- function (text="",text2="",company="",text3="")
{
  div(HTML("<footer class='main-footer'>
  <!-- To the right -->
           <div class='pull-right hidden-xs'>
           "),text,HTML("
           </div>
           <!-- Default to the left -->
           <strong>"),text2,HTML("<a href='#'>"),company,HTML("</a>.</strong>"),text3,HTML("
           </footer>'"))
}

#   NAME: setheader
#   DESCRIPTION: Creates a header, this function should be called inside the ui
#   ARGUMENTS:
#     left: text to display on the left side of the header
#     right: text to display on the right side of the header
#     img: Location or URL where the image is located

setheader <- function (left="",right="",img="")
{
  
  if (is.null(img) || is.na(img) || img=="")
  {
    cnt=right;  
  }else{
    cnt= paste0("<img style='float:right' src='",img,"'>")
  }
  HTML("<div class='logoad' style: width='100%'>
          <div id='idt' class='col-sm-6 left'>",
       left,"  
          </div>
          
          <div class='right-logo col-sm-6'>",
       cnt,"  
          </div>
        </div>")
}