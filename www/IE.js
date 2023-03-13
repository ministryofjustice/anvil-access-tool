function CheckIE()
{
  var browser = navigator.userAgent;
  var browser_index = browser.indexOf("Trident")
  
  var user_agent = window.navigator.userAgent;
  var user_agent_index = ua.indexOf("MSIE ");
  
  if (user_agent_index > 0 || browser_index != -1 || (/MSIE \d|Trident.*rv:/.test(navigator.userAgent))) {
    
    window.alert("It looks like you're using Internet Explorer! The tool will only work in Mozilla Firefox or Google Chrome.");
  }
}

window.onload = CheckIE();
