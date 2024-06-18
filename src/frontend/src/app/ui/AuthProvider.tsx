import { AuthContext } from '../../shared/auth/ui/AuthContext'
import { authToken } from '../../shared/auth/models/authToken'
import { ReactNode, useState } from 'react'
import { redirectToSignIn } from '~/shared/routing'
import { jwtDecode } from 'jwt-decode'
import { UserRoleEnumStr } from '~/shared/auth/UserRoleEnum'
import { TokenPayload } from '~/entities'

interface AuthContextProps {
  children: ReactNode
}

export function AuthProvider({ children }: AuthContextProps) {
  const [authTokenValue, setAuthTokenValue] = useState(authToken.restore())

  return (
    <AuthContext.Provider
      value={{
        authenticated: !!authTokenValue,
        authInfo: authTokenValue
          ? (() => {
              const tokenPayload = jwtDecode(authTokenValue) as TokenPayload

              return {
                tokenPayload,
                user: {
                  isAdmin: UserRoleEnumStr.Admin === tokenPayload.role,
                  isStudent: UserRoleEnumStr.Student === tokenPayload.role,
                  isMentor: UserRoleEnumStr.Mentor === tokenPayload.role,
                  userId: tokenPayload.userId,
                },
              }
            })()
          : null,
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
