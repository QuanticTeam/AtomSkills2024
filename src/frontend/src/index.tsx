import { ConfigProvider } from 'antd'
import React from 'react'
import { createRoot } from 'react-dom/client'
import { RouterProvider, createBrowserRouter } from 'react-router-dom'
import './index.css'
import reportWebVitals from './reportWebVitals'

import { AuthProvider, ProtectedRoute, PublicRoute } from './auth'
import { defaultTheme } from './themes'
import { LoginPage } from './ui/pages/LoginPage'
import { LoremPage } from './ui/pages/LoremPage'
import { RegisterPage } from './ui/pages/RegisterPage'
import { SomethingPage } from './ui/pages/SomethingPage'

const router = createBrowserRouter([
  {
    path: '/',
    element: (
      <ProtectedRoute>
        <SomethingPage />
      </ProtectedRoute>
    ),
  },
  {
    path: 'lorem',
    element: (
      <ProtectedRoute>
        <LoremPage />
      </ProtectedRoute>
    ),
  },
  {
    path: 'login',
    element: (
      <PublicRoute>
        <LoginPage />
      </PublicRoute>
    ),
  },
  {
    path: 'register',
    element: (
      <PublicRoute>
        <RegisterPage />
      </PublicRoute>
    ),
  },
  {
    path: 'something',
    element: (
      <ProtectedRoute>
        <SomethingPage />
      </ProtectedRoute>
    ),
  },
])

createRoot(document.getElementById('root') as HTMLElement).render(
  <React.StrictMode>
    <ConfigProvider theme={defaultTheme}>
      <AuthProvider>
        <RouterProvider router={router} />
      </AuthProvider>
    </ConfigProvider>
  </React.StrictMode>,
)

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals()
