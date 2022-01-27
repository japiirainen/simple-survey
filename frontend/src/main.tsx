import React from 'react'
import ReactDOM from 'react-dom'
import {BrowserRouter} from 'react-router-dom'

ReactDOM.render(
  <React.StrictMode>
    <BrowserRouter>
      <div>hello world</div>
    </BrowserRouter>
  </React.StrictMode>,
  document.getElementById('app')
)
