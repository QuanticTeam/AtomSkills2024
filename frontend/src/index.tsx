import { ConfigProvider } from 'antd'
import React from 'react'
import { createRoot } from 'react-dom/client'
import { RouterProvider, createBrowserRouter } from 'react-router-dom'
import './index.css'
import reportWebVitals from './reportWebVitals'

import { defaultTheme } from './themes'
import { MainPage } from './ui/pages/MainPage'
import { LoginPage } from './ui/pages/LoginPage'
import { RegisterPage } from './ui/pages/RegisterPage'

const router = createBrowserRouter([
  {
    path: '/',
    element: <MainPage />,
  },
  {
    path: 'login',
    element: <LoginPage />,
  },
  {
    path: 'register',
    element: <RegisterPage />,
  },
])

createRoot(document.getElementById('root') as HTMLElement).render(
  <React.StrictMode>
    <ConfigProvider theme={defaultTheme}>
      <RouterProvider router={router} />
    </ConfigProvider>
  </React.StrictMode>,
)

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals()
