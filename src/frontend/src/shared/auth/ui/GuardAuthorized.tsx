import { ReactNode, useContext } from 'react'
import { Navigate } from 'react-router-dom'
import { AuthContext } from './AuthContext'
import { ROUTE_PATH_SIGN_IN } from '~/shared/routng/constants'

interface GuardAuthorizedProps {
  children: ReactNode
}

export function GuardAuthorized({ children }: GuardAuthorizedProps) {
  const { authenticated } = useContext(AuthContext)

  if (authenticated) return <div>{children}</div>

  return (
    <Navigate
      to={ROUTE_PATH_SIGN_IN}
      replace={true}
    />
  )
}
