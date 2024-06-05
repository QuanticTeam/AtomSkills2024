import { AuthContext } from '../../shared/auth/ui/AuthContext'
import { authToken } from '../../shared/auth/models/authToken'
import { message } from 'antd'
import { ReactNode, useState } from 'react'
import { redirectToLogin } from '~/shared/routng'

interface AuthContextProps {
  children: ReactNode
}

export function AuthProvider({ children }: AuthContextProps) {
  const [authTokenValue, setAuthTokenValue] = useState(authToken.restore())
  const [messageApi, contextHolder] = message.useMessage()

  return (
    <AuthContext.Provider
      value={{
        authenticated: !!authTokenValue,
        login(token: NonNullable<typeof authTokenValue>) {
          authToken.store(token)
          setAuthTokenValue(token)
          messageApi.success('Добро пожаловать!')
        },
        logout() {
          authToken.remove()
          setAuthTokenValue(null)
          redirectToLogin()
        },
      }}
    >
      {contextHolder}
      {children}
    </AuthContext.Provider>
  )
}
