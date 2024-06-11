import { AuthContext } from '../../shared/auth/ui/AuthContext'
import { authToken } from '../../shared/auth/models/authToken'
import { ReactNode, useState } from 'react'
import { redirectToSignIn } from '~/shared/routng'

interface AuthContextProps {
  children: ReactNode
}

export function AuthProvider({ children }: AuthContextProps) {
  const [authTokenValue, setAuthTokenValue] = useState(authToken.restore())

  return (
    <AuthContext.Provider
      value={{
        authenticated: !!authTokenValue,
        login(token: NonNullable<typeof authTokenValue>) {
          authToken.store(token)
          setAuthTokenValue(token)
        },
        logout() {
          authToken.remove()
          setAuthTokenValue(null)
          redirectToSignIn()
        },
      }}
    >
      {children}
    </AuthContext.Provider>
  )
}
